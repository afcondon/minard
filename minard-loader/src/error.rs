use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LoaderError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON parse error in {path}: {source}")]
    JsonParse {
        path: PathBuf,
        #[source]
        source: serde_json::Error,
    },

    #[error("Database error: {0}")]
    Database(#[from] duckdb::Error),

    #[error("spago.lock not found at {0}")]
    SpagoLockNotFound(PathBuf),

    #[error("output directory not found at {0}")]
    OutputDirNotFound(PathBuf),

    #[error("Project directory not found: {0}")]
    ProjectNotFound(PathBuf),

    #[error("Invalid spago.lock format: {0}")]
    InvalidSpagoLock(String),
}

pub type Result<T> = std::result::Result<T, LoaderError>;
