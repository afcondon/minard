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

/// Source span from corefn.json (line, column)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SourceSpan {
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

/// A function call extracted from corefn
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionCall {
    pub caller_name: String,
    pub callee_module: String,
    pub callee_name: String,
    pub is_cross_module: bool,
    pub source_span: Option<SourceSpan>,
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
    /// Returns both intra-module and cross-module calls.
    /// Skips:
    /// - Compiler-generated bindings (bind, discard, numbered slots)
    /// - Type class dictionary bindings (local re-bindings of cross-module methods)
    /// - Simple Var re-bindings (Proxy labels, etc.)
    pub fn extract_function_calls(&self) -> Vec<FunctionCall> {
        let mut calls = HashSet::new();
        let self_module = self.module_name_str();

        // Pass 1: identify trivial/compiler-generated declaration names
        // These should be excluded as both callers AND callees
        let mut skip_names: HashSet<String> = HashSet::new();
        for decl in &self.decls {
            if let (Some(identifier), Some(expr)) = (&decl.identifier, &decl.expression) {
                if identifier.starts_with("bind")
                    || identifier.starts_with("discard")
                    || (identifier.starts_with("slot") && identifier.len() < 7)
                    || is_trivial_binding(expr, &self_module)
                {
                    skip_names.insert(identifier.clone());
                }
            }
        }

        // Pass 2: extract calls, skipping trivial callers and callees
        for decl in &self.decls {
            if let (Some(identifier), Some(expr)) = (&decl.identifier, &decl.expression) {
                if skip_names.contains(identifier) {
                    continue;
                }
                extract_calls_from_expr(identifier, &self_module, expr, &mut calls, &skip_names);
            }
        }

        calls.into_iter().collect()
    }
}

/// Check if a declaration's expression is a trivial binding:
/// - Simple Var re-binding (e.g. `_fooViz = Proxy`)
/// - Dictionary application (e.g. `eq = Data.Eq.eq(eqInstance)`)
/// These are compiler-generated plumbing, not user-written functions.
fn is_trivial_binding(expr: &Value, self_module: &str) -> bool {
    match expr {
        Value::Object(obj) => {
            let expr_type = obj.get("type").and_then(|t| t.as_str()).unwrap_or("");

            // Simple Var re-binding
            if expr_type == "Var" {
                return true;
            }

            // App of a cross-module Var (dictionary application)
            if expr_type == "App" {
                if let Some(Value::Object(fn_obj)) = obj.get("abstraction") {
                    let fn_type = fn_obj.get("type").and_then(|t| t.as_str()).unwrap_or("");
                    if fn_type == "Var" {
                        let fn_mod = fn_obj
                            .get("value")
                            .and_then(|v| v.get("moduleName"))
                            .and_then(|m| m.as_array())
                            .map(|arr| {
                                arr.iter()
                                    .filter_map(|v| v.as_str())
                                    .collect::<Vec<_>>()
                                    .join(".")
                            })
                            .unwrap_or_default();
                        if fn_mod != self_module {
                            return true;
                        }
                    }
                    // Nested App(App(Var, ...), ...) for multi-arg dictionary applications
                    if fn_type == "App" {
                        if let Some(Value::Object(inner_fn)) = fn_obj.get("abstraction") {
                            let inner_type =
                                inner_fn.get("type").and_then(|t| t.as_str()).unwrap_or("");
                            if inner_type == "Var" {
                                let inner_mod = inner_fn
                                    .get("value")
                                    .and_then(|v| v.get("moduleName"))
                                    .and_then(|m| m.as_array())
                                    .map(|arr| {
                                        arr.iter()
                                            .filter_map(|v| v.as_str())
                                            .collect::<Vec<_>>()
                                            .join(".")
                                    })
                                    .unwrap_or_default();
                                if inner_mod != self_module {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Extract source span from a corefn annotation object
fn extract_source_span(obj: &serde_json::Map<String, Value>) -> Option<SourceSpan> {
    let ann = obj.get("annotation")?.as_object()?;
    let span = ann.get("sourceSpan")?.as_object()?;
    let start = span.get("start")?.as_array()?;
    let end = span.get("end")?.as_array()?;
    Some(SourceSpan {
        start_line: start.first()?.as_u64()? as u32,
        start_col: start.get(1)?.as_u64()? as u32,
        end_line: end.first()?.as_u64()? as u32,
        end_col: end.get(1)?.as_u64()? as u32,
    })
}

/// Recursively extract function calls from a CoreFn expression
fn extract_calls_from_expr(
    caller_name: &str,
    self_module: &str,
    expr: &Value,
    calls: &mut HashSet<FunctionCall>,
    skip_names: &HashSet<String>,
) {
    match expr {
        Value::Object(obj) => {
            // Check if this is a Var reference
            if let (Some(Value::String(typ)), Some(value)) = (obj.get("type"), obj.get("value")) {
                if typ == "Var" {
                    // Check meta type — skip constructors and newtypes (not real function calls)
                    let meta_type = obj
                        .get("annotation")
                        .and_then(|a| a.get("meta"))
                        .and_then(|m| m.get("metaType"))
                        .and_then(|mt| mt.as_str())
                        .unwrap_or("");

                    if meta_type != "IsConstructor" && meta_type != "IsNewtype" {
                        if let Value::Object(val_obj) = value {
                            if let (Some(Value::String(id)), Some(Value::Array(mod_name))) =
                                (val_obj.get("identifier"), val_obj.get("moduleName"))
                            {
                                let callee_module: String = mod_name
                                    .iter()
                                    .filter_map(|v| v.as_str())
                                    .collect::<Vec<_>>()
                                    .join(".");

                                // Skip calls to trivial/compiler-generated declarations
                                // (within same module — cross-module trivials don't affect structure)
                                let is_trivial_callee = callee_module == self_module
                                    && skip_names.contains(id.as_str());

                                if !is_trivial_callee {
                                    // Skip self-calls (same function calling itself)
                                    if !(callee_module == self_module && id == caller_name) {
                                        let is_cross_module = callee_module != self_module;
                                        let source_span = extract_source_span(obj);
                                        calls.insert(FunctionCall {
                                            caller_name: caller_name.to_string(),
                                            callee_module,
                                            callee_name: id.clone(),
                                            is_cross_module,
                                            source_span,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Recurse into all object values
            for (_, v) in obj {
                extract_calls_from_expr(caller_name, self_module, v, calls, skip_names);
            }
        }
        Value::Array(arr) => {
            for v in arr {
                extract_calls_from_expr(caller_name, self_module, v, calls, skip_names);
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
        assert!(calls[0].is_cross_module);
    }
}
