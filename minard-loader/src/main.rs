use anyhow::Context;
use clap::Parser;
use duckdb::Connection;
use std::fs;
use std::time::Instant;

use minard_loader::{
    db::{drop_all_tables, get_stats, init_schema},
    loader::{discovery::discover_all, LoadPipeline, ProjectDiscovery},
    model::ScanStats,
    progress::ProgressReporter,
    Cli, Commands,
};

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { fresh, database } => {
            println!("Initializing database: {}", database.display());

            if fresh && database.exists() {
                println!("Removing existing database (--fresh)...");
                fs::remove_file(&database)?;
                // Also remove WAL file if exists
                let wal = database.with_extension("duckdb.wal");
                if wal.exists() {
                    fs::remove_file(&wal)?;
                }
            }

            let conn = Connection::open(&database)
                .with_context(|| format!("Failed to open database: {}", database.display()))?;

            if !fresh {
                // Drop tables if they exist (in case of partial init)
                drop_all_tables(&conn)?;
            }

            init_schema(&conn)?;

            println!("Database initialized successfully!");
        }

        Commands::Load {
            project_path,
            database,
            project,
            label,
            scan,
            verbose,
            quiet,
        } => {
            // Initialize database if it doesn't exist
            if !database.exists() {
                if !quiet {
                    println!("Database not found, initializing...");
                }
                let conn = Connection::open(&database)?;
                init_schema(&conn)?;
            }

            let conn = Connection::open(&database)
                .with_context(|| format!("Failed to open database: {}", database.display()))?;

            let progress = ProgressReporter::new(quiet);
            let pipeline = LoadPipeline::new(verbose);

            // Initialize ID generator from database
            pipeline.init_ids(&conn)?;

            if scan {
                // Scan mode: discover and load all projects
                load_scan(
                    &conn,
                    &project_path,
                    label.as_deref(),
                    &pipeline,
                    &progress,
                    verbose,
                    quiet,
                )?;
            } else {
                // Single project mode
                load_single(
                    &conn,
                    &project_path,
                    project.as_deref(),
                    label.as_deref(),
                    &pipeline,
                    &progress,
                    quiet,
                )?;
            }

            progress.finish();
        }

        Commands::Stats { database } => {
            if !database.exists() {
                anyhow::bail!("Database not found: {}", database.display());
            }

            let conn = Connection::open(&database)
                .with_context(|| format!("Failed to open database: {}", database.display()))?;

            let stats = get_stats(&conn)?;
            println!("{}", stats.report());
        }
    }

    Ok(())
}

/// Load a single project
fn load_single(
    conn: &Connection,
    project_path: &std::path::Path,
    project_name: Option<&str>,
    label: Option<&str>,
    pipeline: &LoadPipeline,
    progress: &ProgressReporter,
    quiet: bool,
) -> anyhow::Result<()> {
    let discovery = ProjectDiscovery::discover(project_path)
        .with_context(|| format!("Failed to discover project: {}", project_path.display()))?;

    let name = project_name
        .map(|s| s.to_string())
        .unwrap_or_else(|| discovery.project_name());

    if !quiet {
        println!("Loading project '{}' from {}", name, project_path.display());
    }

    let stats = pipeline
        .load(conn, &discovery, &name, label, progress)
        .with_context(|| format!("Failed to load project: {}", project_path.display()))?;

    if !quiet {
        println!("\n{}", stats.report());
    }

    Ok(())
}

/// Scan and load all projects in a directory tree
fn load_scan(
    conn: &Connection,
    root_path: &std::path::Path,
    label: Option<&str>,
    pipeline: &LoadPipeline,
    progress: &ProgressReporter,
    verbose: bool,
    quiet: bool,
) -> anyhow::Result<()> {
    let start = Instant::now();

    if !quiet {
        println!("Scanning for projects in {}...", root_path.display());
    }

    let discoveries = discover_all(root_path);

    if discoveries.is_empty() {
        anyhow::bail!(
            "No PureScript projects found in {}",
            root_path.display()
        );
    }

    if !quiet {
        println!("Found {} projects with built output", discoveries.len());
    }

    let mut scan_stats = ScanStats::default();

    for discovery in &discoveries {
        let name = discovery.project_name();

        if verbose {
            println!(
                "\nLoading {} ({} modules)...",
                name,
                discovery.module_count()
            );
        }

        match pipeline.load(conn, discovery, &name, label, progress) {
            Ok(stats) => {
                scan_stats.add(&stats);
                if verbose {
                    println!("  {}", stats.report());
                }
            }
            Err(e) => {
                scan_stats.projects_skipped += 1;
                if verbose {
                    eprintln!("  Warning: Failed to load {}: {}", name, e);
                }
            }
        }
    }

    scan_stats.elapsed_ms = start.elapsed().as_millis() as u64;

    if !quiet {
        println!("\n{}", scan_stats.report());
    }

    Ok(())
}
