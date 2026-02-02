use serde::Deserialize;
use serde_json::Value;
use std::collections::HashSet;
use std::fs;
use std::path::Path;

use crate::error::{LoaderError, Result};

/// Represents the corefn.json file structure (minimal extraction)
#[derive(Debug, Deserialize)]
pub struct CoreFn {
    #[serde(rename = "moduleName")]
    pub module_name: Vec<String>,
    #[serde(default)]
    pub imports: Vec<CoreFnImport>,
    #[serde(default)]
    pub decls: Vec<CoreFnDecl>,
}

#[derive(Debug, Deserialize)]
pub struct CoreFnImport {
    #[serde(rename = "moduleName")]
    pub module_name: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct CoreFnDecl {
    pub identifier: Option<String>,
    #[serde(rename = "bindType")]
    pub bind_type: Option<String>,
    pub expression: Option<Value>,
    pub annotation: Option<CoreFnAnnotation>,
}

#[derive(Debug, Deserialize)]
pub struct CoreFnAnnotation {
    #[serde(rename = "sourceSpan")]
    pub source_span: Option<CoreFnSourceSpan>,
}

#[derive(Debug, Deserialize)]
pub struct CoreFnSourceSpan {
    pub start: [u32; 2],
    pub end: [u32; 2],
}

/// A function call extracted from corefn
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionCall {
    pub caller_name: String,
    pub callee_module: String,
    pub callee_name: String,
}

impl CoreFn {
    /// Parse a corefn.json file from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path)?;
        Self::from_str(&content, path)
    }

    /// Parse corefn.json from a string
    pub fn from_str(content: &str, path: &Path) -> Result<Self> {
        serde_json::from_str(content).map_err(|e| LoaderError::JsonParse {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Get the module name as a dotted string
    pub fn module_name_str(&self) -> String {
        self.module_name.join(".")
    }

    /// Get all imported module names as dotted strings
    pub fn imported_modules(&self) -> Vec<String> {
        self.imports
            .iter()
            .map(|i| i.module_name.join("."))
            .filter(|m| m != &self.module_name_str()) // Exclude self-import
            .collect()
    }

    /// Compute lines of code by finding the maximum end line across all declarations
    /// This is more accurate than docs.json because it includes internal (non-exported) declarations
    pub fn compute_loc(&self) -> Option<i32> {
        let mut max_line: u32 = 0;

        for decl in &self.decls {
            if let Some(ann) = &decl.annotation {
                if let Some(span) = &ann.source_span {
                    if span.end[0] > max_line {
                        max_line = span.end[0];
                    }
                }
            }
        }

        if max_line > 0 {
            Some(max_line as i32)
        } else {
            None
        }
    }

    /// Extract function calls from declarations
    /// Returns calls where the callee is from a different module
    pub fn extract_function_calls(&self) -> Vec<FunctionCall> {
        let mut calls = HashSet::new();
        let self_module = self.module_name_str();

        for decl in &self.decls {
            if let (Some(identifier), Some(expr)) = (&decl.identifier, &decl.expression) {
                extract_calls_from_expr(identifier, &self_module, expr, &mut calls);
            }
        }

        calls.into_iter().collect()
    }
}

/// Recursively extract function calls from a CoreFn expression
fn extract_calls_from_expr(
    caller_name: &str,
    self_module: &str,
    expr: &Value,
    calls: &mut HashSet<FunctionCall>,
) {
    match expr {
        Value::Object(obj) => {
            // Check if this is a Var reference
            if let (Some(Value::String(typ)), Some(value)) = (obj.get("type"), obj.get("value")) {
                if typ == "Var" {
                    if let Value::Object(val_obj) = value {
                        if let (Some(Value::String(id)), Some(Value::Array(mod_name))) =
                            (val_obj.get("identifier"), val_obj.get("moduleName"))
                        {
                            let callee_module: String = mod_name
                                .iter()
                                .filter_map(|v| v.as_str())
                                .collect::<Vec<_>>()
                                .join(".");

                            // Only record cross-module calls
                            if callee_module != self_module {
                                calls.insert(FunctionCall {
                                    caller_name: caller_name.to_string(),
                                    callee_module,
                                    callee_name: id.clone(),
                                });
                            }
                        }
                    }
                }
            }

            // Recurse into all object values
            for (_, v) in obj {
                extract_calls_from_expr(caller_name, self_module, v, calls);
            }
        }
        Value::Array(arr) => {
            for v in arr {
                extract_calls_from_expr(caller_name, self_module, v, calls);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_imports() {
        let sample = r#"{
            "moduleName": ["App"],
            "imports": [
                {"moduleName": ["App"], "annotation": {}},
                {"moduleName": ["Data", "Maybe"], "annotation": {}},
                {"moduleName": ["Effect"], "annotation": {}}
            ],
            "decls": []
        }"#;

        let corefn =
            CoreFn::from_str(sample, Path::new("test.json")).expect("Failed to parse");

        assert_eq!(corefn.module_name_str(), "App");

        let imports = corefn.imported_modules();
        assert_eq!(imports.len(), 2); // App self-import excluded
        assert!(imports.contains(&"Data.Maybe".to_string()));
        assert!(imports.contains(&"Effect".to_string()));
    }

    #[test]
    fn test_extract_function_calls() {
        // Sample with a simple function call
        let sample = r#"{
            "moduleName": ["App"],
            "imports": [],
            "decls": [{
                "identifier": "myFunc",
                "bindType": "NonRec",
                "expression": {
                    "type": "Var",
                    "value": {
                        "identifier": "map",
                        "moduleName": ["Data", "Functor"]
                    }
                }
            }]
        }"#;

        let corefn = CoreFn::from_str(sample, Path::new("test.json")).expect("Failed to parse");
        let calls = corefn.extract_function_calls();

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].caller_name, "myFunc");
        assert_eq!(calls[0].callee_module, "Data.Functor");
        assert_eq!(calls[0].callee_name, "map");
    }
}
