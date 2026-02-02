use indicatif::{ProgressBar, ProgressStyle};

/// Progress reporter for CLI feedback
pub struct ProgressReporter {
    bar: ProgressBar,
    quiet: bool,
}

impl ProgressReporter {
    pub fn new(quiet: bool) -> Self {
        let bar = if quiet {
            ProgressBar::hidden()
        } else {
            ProgressBar::new_spinner()
        };

        bar.set_style(
            ProgressStyle::default_spinner()
                .template("{spinner:.green} {msg}")
                .unwrap(),
        );

        Self { bar, quiet }
    }

    pub fn set_message(&self, msg: &str) {
        if !self.quiet {
            self.bar.set_message(msg.to_string());
        }
    }

    pub fn set_total(&self, total: u64) {
        if !self.quiet {
            self.bar.set_length(total);
            self.bar.set_style(
                ProgressStyle::default_bar()
                    .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} {msg}")
                    .unwrap()
                    .progress_chars("=>-"),
            );
        }
    }

    pub fn inc(&self, delta: u64) {
        self.bar.inc(delta);
    }

    pub fn finish(&self) {
        self.bar.finish_and_clear();
    }

    pub fn println(&self, msg: &str) {
        if !self.quiet {
            self.bar.println(msg);
        }
    }
}

impl Drop for ProgressReporter {
    fn drop(&mut self) {
        self.bar.finish_and_clear();
    }
}
