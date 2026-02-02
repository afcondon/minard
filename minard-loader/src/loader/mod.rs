pub mod detect;
pub mod discovery;
pub mod pipeline;
pub mod postload;

pub use detect::{detect_backend, scan_ffi_files, scan_package_ffi};
pub use discovery::ProjectDiscovery;
pub use pipeline::LoadPipeline;
pub use postload::*;
