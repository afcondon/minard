use std::path::Path;
use std::process::Command;

/// Git information for a repository
#[derive(Debug, Clone)]
pub struct GitInfo {
    pub hash: String,
    pub ref_name: Option<String>,
}

/// Get current git info for a path
pub fn get_git_info(path: &Path) -> Option<GitInfo> {
    // Get the commit hash
    let hash = get_git_hash(path)?;

    // Try to get a ref name (branch or tag)
    let ref_name = get_git_ref(path);

    Some(GitInfo { hash, ref_name })
}

/// Get the current git commit hash
fn get_git_hash(path: &Path) -> Option<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(path)
        .output()
        .ok()?;

    if output.status.success() {
        let hash = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !hash.is_empty() {
            return Some(hash);
        }
    }
    None
}

/// Get the current git ref (branch name or tag)
fn get_git_ref(path: &Path) -> Option<String> {
    // First try to get the branch name
    let branch_output = Command::new("git")
        .args(["symbolic-ref", "--short", "HEAD"])
        .current_dir(path)
        .output()
        .ok();

    if let Some(output) = branch_output {
        if output.status.success() {
            let ref_name = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !ref_name.is_empty() {
                return Some(ref_name);
            }
        }
    }

    // Fall back to describe (tags)
    let describe_output = Command::new("git")
        .args(["describe", "--tags", "--exact-match"])
        .current_dir(path)
        .output()
        .ok();

    if let Some(output) = describe_output {
        if output.status.success() {
            let ref_name = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !ref_name.is_empty() {
                return Some(ref_name);
            }
        }
    }

    None
}

/// Check if a path is inside a git repository
pub fn is_git_repo(path: &Path) -> bool {
    Command::new("git")
        .args(["rev-parse", "--git-dir"])
        .current_dir(path)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_current_dir_is_git() {
        // This test assumes we're running in a git repo
        let cwd = env::current_dir().unwrap();
        // May or may not be a git repo depending on where tests run
        let _ = is_git_repo(&cwd);
    }
}
