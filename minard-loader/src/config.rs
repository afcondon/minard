use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "minard-loader")]
#[command(about = "Fast Rust loader for PureScript project data into DuckDB")]
#[command(version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Initialize the database schema
    Init {
        /// Remove existing database and create fresh
        #[arg(long)]
        fresh: bool,

        /// Path to database file
        #[arg(short, long, default_value = "minard.duckdb")]
        database: PathBuf,
    },

    /// Load a PureScript project into the database
    Load {
        /// Path to the PureScript project directory (or root for --scan)
        project_path: PathBuf,

        /// Path to database file
        #[arg(short, long, default_value = "minard.duckdb")]
        database: PathBuf,

        /// Project name (defaults to directory name)
        #[arg(short = 'p', long)]
        project: Option<String>,

        /// Snapshot label (defaults to git ref or "manual")
        #[arg(short, long)]
        label: Option<String>,

        /// Scan for all spago.lock files in subdirectories
        #[arg(long)]
        scan: bool,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,

        /// Quiet mode (no progress output)
        #[arg(short, long)]
        quiet: bool,
    },

    /// Show database statistics
    Stats {
        /// Path to database file
        #[arg(short, long, default_value = "minard.duckdb")]
        database: PathBuf,
    },
}
