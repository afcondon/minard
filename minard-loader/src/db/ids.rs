use std::sync::atomic::{AtomicI64, Ordering};

use super::insert::MaxIds;

/// Thread-safe ID generator for parallel processing
#[derive(Debug)]
pub struct IdGenerator {
    project_counter: AtomicI64,
    snapshot_counter: AtomicI64,
    package_counter: AtomicI64,
    namespace_counter: AtomicI64,
    module_counter: AtomicI64,
    declaration_counter: AtomicI64,
    child_counter: AtomicI64,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            project_counter: AtomicI64::new(0),
            snapshot_counter: AtomicI64::new(0),
            package_counter: AtomicI64::new(0),
            namespace_counter: AtomicI64::new(0),
            module_counter: AtomicI64::new(0),
            declaration_counter: AtomicI64::new(0),
            child_counter: AtomicI64::new(0),
        }
    }

    /// Initialize counters from existing database max IDs
    pub fn init_from_db(&self, max_ids: &MaxIds) {
        self.project_counter.store(max_ids.project, Ordering::SeqCst);
        self.snapshot_counter.store(max_ids.snapshot, Ordering::SeqCst);
        self.package_counter.store(max_ids.package, Ordering::SeqCst);
        self.namespace_counter.store(max_ids.namespace, Ordering::SeqCst);
        self.module_counter.store(max_ids.module, Ordering::SeqCst);
        self.declaration_counter.store(max_ids.declaration, Ordering::SeqCst);
        self.child_counter.store(max_ids.child, Ordering::SeqCst);
    }

    pub fn next_project_id(&self) -> i64 {
        self.project_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_snapshot_id(&self) -> i64 {
        self.snapshot_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_package_id(&self) -> i64 {
        self.package_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_namespace_id(&self) -> i64 {
        self.namespace_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_module_id(&self) -> i64 {
        self.module_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_declaration_id(&self) -> i64 {
        self.declaration_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    pub fn next_child_id(&self) -> i64 {
        self.child_counter.fetch_add(1, Ordering::SeqCst) + 1
    }

    /// Reserve a batch of IDs for parallel processing
    /// Returns start_id where IDs are start_id..start_id+count
    pub fn reserve_module_ids(&self, count: i64) -> i64 {
        self.module_counter.fetch_add(count, Ordering::SeqCst) + 1
    }

    pub fn reserve_declaration_ids(&self, count: i64) -> i64 {
        self.declaration_counter.fetch_add(count, Ordering::SeqCst) + 1
    }

    pub fn reserve_child_ids(&self, count: i64) -> i64 {
        self.child_counter.fetch_add(count, Ordering::SeqCst) + 1
    }

    /// Get current namespace counter value (for passing to get_or_create_namespace)
    pub fn current_namespace_id(&self) -> i64 {
        self.namespace_counter.load(Ordering::SeqCst)
    }

    /// Update namespace counter after batch operations
    pub fn set_namespace_counter(&self, value: i64) {
        self.namespace_counter.store(value, Ordering::SeqCst);
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}
