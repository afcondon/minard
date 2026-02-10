use anyhow::Result;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

#[derive(Deserialize)]
struct CstSpansEntry {
    path: String,
    #[serde(rename = "moduleName")]
    #[allow(dead_code)]
    module_name: String,
    declarations: Vec<CstDeclEntry>,
}

#[derive(Deserialize)]
struct CstDeclEntry {
    name: String,
    #[allow(dead_code)]
    kind: String,
    span: CstSpan,
}

#[derive(Deserialize)]
struct CstSpan {
    start: CstPos,
    end: CstPos,
}

#[derive(Deserialize)]
struct CstPos {
    line: u32,
    #[allow(dead_code)]
    column: u32,
}

/// Index of CST-derived declaration spans.
/// Keyed by (relative_file_path, declaration_name) -> (start_line, end_line).
pub struct CstSpanIndex {
    spans: HashMap<(String, String), (u32, u32)>,
}

impl CstSpanIndex {
    /// Load CST spans from a JSON file produced by minard-cst.
    pub fn from_path(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let entries: Vec<CstSpansEntry> = serde_json::from_str(&content)?;

        let mut spans = HashMap::new();
        for entry in entries {
            for decl in entry.declarations {
                spans.insert(
                    (entry.path.clone(), decl.name.clone()),
                    (decl.span.start.line, decl.span.end.line),
                );
            }
        }

        Ok(Self { spans })
    }

    /// Look up the full span for a declaration by file path and name.
    pub fn get_span(&self, file_path: &str, decl_name: &str) -> Option<(u32, u32)> {
        self.spans
            .get(&(file_path.to_string(), decl_name.to_string()))
            .copied()
    }

    /// Create an empty index (no CST spans available).
    pub fn empty() -> Self {
        Self {
            spans: HashMap::new(),
        }
    }

    /// Number of declaration spans in the index.
    pub fn len(&self) -> usize {
        self.spans.len()
    }
}
