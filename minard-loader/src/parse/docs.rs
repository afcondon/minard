use serde::Deserialize;
use serde_json::Value;
use std::fs;
use std::path::Path;

use crate::error::{LoaderError, Result};

/// Represents a docs.json file structure
#[derive(Debug, Deserialize)]
pub struct DocsJson {
    pub name: String,
    pub comments: Option<String>,
    pub declarations: Vec<Declaration>,
    #[serde(rename = "reExports", default)]
    pub re_exports: Vec<ReExport>,
}

#[derive(Debug, Deserialize)]
pub struct ReExport {
    pub module: Option<String>,
    pub declarations: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub struct Declaration {
    pub title: String,
    pub comments: Option<String>,
    pub info: DeclarationInfo,
    #[serde(rename = "sourceSpan")]
    pub source_span: Option<SourceSpan>,
    #[serde(default)]
    pub children: Vec<ChildDeclaration>,
    pub kind: Option<Value>,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "declType")]
pub enum DeclarationInfo {
    #[serde(rename = "value")]
    Value {
        #[serde(rename = "type")]
        type_info: Option<Value>,
    },
    #[serde(rename = "data")]
    Data {
        #[serde(rename = "dataDeclType")]
        data_decl_type: Option<String>,
        #[serde(rename = "typeArguments", default)]
        type_arguments: Vec<TypeArgument>,
        #[serde(default)]
        roles: Vec<String>,
    },
    #[serde(rename = "newtype")]
    Newtype {
        #[serde(rename = "dataDeclType")]
        data_decl_type: Option<String>,
        #[serde(rename = "typeArguments", default)]
        type_arguments: Vec<TypeArgument>,
        #[serde(default)]
        roles: Vec<String>,
    },
    #[serde(rename = "typeSynonym")]
    TypeSynonym {
        #[serde(rename = "type")]
        type_info: Option<Value>,
        #[serde(rename = "typeArguments", default)]
        type_arguments: Vec<TypeArgument>,
    },
    #[serde(rename = "typeClass")]
    TypeClass {
        #[serde(rename = "typeArguments", default)]
        type_arguments: Vec<TypeArgument>,
        #[serde(default)]
        superclasses: Vec<Value>,
        #[serde(default)]
        fundeps: Vec<Value>,
    },
    #[serde(rename = "alias")]
    Alias {
        #[serde(rename = "type")]
        type_info: Option<Value>,
    },
    #[serde(rename = "externalValue")]
    ExternalValue {
        #[serde(rename = "type")]
        type_info: Option<Value>,
    },
    #[serde(other)]
    Unknown,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(untagged)]
pub enum TypeArgument {
    Named([Value; 2]), // [name, kind]
    Value(Value),
}

#[derive(Debug, Deserialize)]
pub struct ChildDeclaration {
    pub title: String,
    pub comments: Option<String>,
    pub info: ChildDeclarationInfo,
    #[serde(rename = "sourceSpan")]
    pub source_span: Option<SourceSpan>,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "declType")]
pub enum ChildDeclarationInfo {
    #[serde(rename = "dataConstructor")]
    DataConstructor {
        #[serde(default)]
        arguments: Vec<Value>,
    },
    #[serde(rename = "instance")]
    Instance {
        #[serde(rename = "type")]
        type_info: Option<Value>,
        #[serde(default)]
        dependencies: Vec<Value>,
    },
    #[serde(rename = "typeClassMember")]
    TypeClassMember {
        #[serde(rename = "type")]
        type_info: Option<Value>,
    },
    #[serde(other)]
    Unknown,
}

#[derive(Debug, Deserialize, Clone)]
pub struct SourceSpan {
    pub start: [u32; 2],
    pub end: [u32; 2],
    pub name: Option<String>,
}

impl DocsJson {
    /// Parse a docs.json file from a path
    pub fn from_path(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path)?;
        Self::from_str(&content, path)
    }

    /// Parse docs.json from a string
    pub fn from_str(content: &str, path: &Path) -> Result<Self> {
        serde_json::from_str(content).map_err(|e| LoaderError::JsonParse {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Check if this module belongs to THIS package (from src/) vs a dependency
    /// Returns true only if the module's source path starts with "src/" or "test/"
    /// (not "../" for workspace deps or ".spago/" for registry deps)
    pub fn is_local_module(&self) -> bool {
        // Check the first declaration's source_span to determine if this is a local module
        if let Some(decl) = self.declarations.first() {
            if let Some(ref span) = decl.source_span {
                if let Some(ref name) = span.name {
                    // Only count as local if path starts with src/ or test/
                    // Exclude:
                    // - ".spago/p/prelude-6.0.1/src/..." (registry deps)
                    // - "../purescript-hylograph-selection/src/..." (workspace deps)
                    return name.starts_with("src/") || name.starts_with("test/");
                }
            }
        }
        // If we can't determine (no declarations or no source_span), exclude it
        false
    }

    /// Get the source file path from the first declaration's source_span
    pub fn get_source_path(&self) -> Option<String> {
        self.declarations
            .first()
            .and_then(|decl| decl.source_span.as_ref())
            .and_then(|span| span.name.clone())
    }

    /// Compute approximate lines of code by finding the maximum end line
    /// across all declarations and child declarations
    pub fn compute_loc(&self) -> Option<i32> {
        let mut max_line: u32 = 0;

        for decl in &self.declarations {
            if let Some(ref span) = decl.source_span {
                // end is [line, column], we want the line (index 0)
                if span.end[0] > max_line {
                    max_line = span.end[0];
                }
            }

            // Also check child declarations
            for child in &decl.children {
                if let Some(ref span) = child.source_span {
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
}

impl Declaration {
    /// Get the declaration kind as a string
    pub fn kind_str(&self) -> &'static str {
        match &self.info {
            DeclarationInfo::Value { .. } => "value",
            DeclarationInfo::Data { .. } => "data",
            DeclarationInfo::Newtype { .. } => "newtype",
            DeclarationInfo::TypeSynonym { .. } => "type_synonym",
            DeclarationInfo::TypeClass { .. } => "type_class",
            DeclarationInfo::Alias { .. } => "alias",
            DeclarationInfo::ExternalValue { .. } => "foreign",
            DeclarationInfo::Unknown => "unknown",
        }
    }

    /// Get the type AST if this declaration has one
    pub fn type_ast(&self) -> Option<&Value> {
        match &self.info {
            DeclarationInfo::Value { type_info } => type_info.as_ref(),
            DeclarationInfo::TypeSynonym { type_info, .. } => type_info.as_ref(),
            DeclarationInfo::Alias { type_info } => type_info.as_ref(),
            DeclarationInfo::ExternalValue { type_info } => type_info.as_ref(),
            _ => None,
        }
    }

    /// Get data declaration type (data vs newtype)
    pub fn data_decl_type(&self) -> Option<&str> {
        match &self.info {
            DeclarationInfo::Data { data_decl_type, .. } => {
                data_decl_type.as_ref().map(|s| s.as_str())
            }
            DeclarationInfo::Newtype { data_decl_type, .. } => {
                data_decl_type.as_ref().map(|s| s.as_str())
            }
            _ => None,
        }
    }

    /// Get type arguments
    pub fn type_arguments(&self) -> Option<&Vec<TypeArgument>> {
        match &self.info {
            DeclarationInfo::Data { type_arguments, .. } => Some(type_arguments),
            DeclarationInfo::Newtype { type_arguments, .. } => Some(type_arguments),
            DeclarationInfo::TypeSynonym { type_arguments, .. } => Some(type_arguments),
            DeclarationInfo::TypeClass { type_arguments, .. } => Some(type_arguments),
            _ => None,
        }
    }

    /// Get roles (for data types)
    pub fn roles(&self) -> Option<&Vec<String>> {
        match &self.info {
            DeclarationInfo::Data { roles, .. } => Some(roles),
            DeclarationInfo::Newtype { roles, .. } => Some(roles),
            _ => None,
        }
    }

    /// Get superclasses (for type classes)
    pub fn superclasses(&self) -> Option<&Vec<Value>> {
        match &self.info {
            DeclarationInfo::TypeClass { superclasses, .. } => Some(superclasses),
            _ => None,
        }
    }

    /// Get functional dependencies (for type classes)
    pub fn fundeps(&self) -> Option<&Vec<Value>> {
        match &self.info {
            DeclarationInfo::TypeClass { fundeps, .. } => Some(fundeps),
            _ => None,
        }
    }
}

impl ChildDeclaration {
    /// Get the child declaration kind as a string
    pub fn kind_str(&self) -> &'static str {
        match &self.info {
            ChildDeclarationInfo::DataConstructor { .. } => "constructor",
            ChildDeclarationInfo::Instance { .. } => "instance",
            ChildDeclarationInfo::TypeClassMember { .. } => "class_member",
            ChildDeclarationInfo::Unknown => "unknown",
        }
    }

    /// Get constructor arguments
    pub fn constructor_args(&self) -> Option<&Vec<Value>> {
        match &self.info {
            ChildDeclarationInfo::DataConstructor { arguments } => Some(arguments),
            _ => None,
        }
    }

    /// Get instance type
    pub fn instance_type(&self) -> Option<&Value> {
        match &self.info {
            ChildDeclarationInfo::Instance { type_info, .. } => type_info.as_ref(),
            _ => None,
        }
    }

    /// Get instance constraints/dependencies
    pub fn instance_constraints(&self) -> Option<&Vec<Value>> {
        match &self.info {
            ChildDeclarationInfo::Instance { dependencies, .. } => Some(dependencies),
            _ => None,
        }
    }

    /// Get type class member type
    pub fn member_type(&self) -> Option<&Value> {
        match &self.info {
            ChildDeclarationInfo::TypeClassMember { type_info } => type_info.as_ref(),
            _ => None,
        }
    }
}
