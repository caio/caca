#![forbid(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::flat_map_option,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::from_iter_instead_of_collect,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_digit_groups,
    clippy::large_stack_arrays,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wild_err_arm,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::missing_enforced_import_renames,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::needless_for_each,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::rc_mutex,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::single_match_else,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]

use std::{
    num::NonZeroUsize,
    path::{Path, PathBuf},
    sync::Arc,
};

use tokio::net::TcpListener as AsyncTcpListener;

use axum::{
    extract::{Path as ReqPath, State},
    http::{StatusCode, Uri},
    response::{IntoResponse, Response},
    routing::{get, post},
    Router,
};

use tower_http::{limit::RequestBodyLimitLayer, trace::TraceLayer};

use tracing_subscriber::{
    filter::{EnvFilter, LevelFilter},
    fmt,
    prelude::*,
};

mod admin;
mod client;
mod config;
mod metadata;
mod repo;
mod view;

use crate::{client::Client, config::GlobalConfig, repo::RepoState, view::Theme};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>; // yolo

fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(fmt::Layer::default().compact().without_time())
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let config = Arc::new(
        GlobalConfig {
            site: config::Site {
                listing_title: String::from("caio's code asylum"),
                listing_html_header: String::from("<h1>caca</h1>"),
                base_url: String::from("http://localhost:42080"),
                clone_base_url: None,
                // to allow mounting caca as a subdirectory
                // when set, a repo url is base_url + reverse_proxy_base + / + name
                reverse_proxy_base: None,
            },
            max_file_size_bytes: 2 * 1024 * 1024,
            rename_similarity_threshold: Some(0.7),
            repo_object_cache_size: Some(20 * 1024 * 1024),
            metadata_config: Some(config::MetadataConfig::default()),
            mailmap_config: config::MailmapConfig::default(),
            global_mailmap: None,
            feed_size: NonZeroUsize::new(40),
            log_size: NonZeroUsize::new(30).unwrap(),
            allow_http_clone: true,
            cache_size: NonZeroUsize::new(1000).unwrap(),
            // theme: Theme::Static,
            theme: Theme::AutoReload("caca/theme".to_string()),
            num_threads: None,
            export_all: true, // false => require git-daemon-export-ok
            listen_mode: config::ListenMode::addr("[::]:42080")?,
            // listen_mode: config::ListenMode::with_admin("[::]:42080", "[::1]:42081")?,
            // listen_mode: config::ListenMode::external(),
        }
        .check()?,
    );

    // May fiddle with env. keep it early at boot
    let (app_listener, admin_listener) = config.listen_mode.to_non_blocking_sockets()?;

    let num_threads = config
        .num_threads
        .unwrap_or(std::thread::available_parallelism()?.get());

    let pool = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .thread_name(|number| format!("caca-cpu-{number:02}"))
            .num_threads(num_threads)
            .build()?,
    );

    let basedir =
        PathBuf::from(std::env::args().nth(1).expect("path as first arg")).canonicalize()?;

    let repos = open_repos(basedir.clone(), &pool, Arc::clone(&config))?;
    if repos.is_empty() {
        // XXX not too difficult to allow admin add
        return Err("No git repositories found".into());
    }
    tracing::info!("{} repositories loaded", repos.len());

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    rt.block_on(async move {
        let app_listener = AsyncTcpListener::from_std(app_listener)?;
        let admin_listener = admin_listener.map(AsyncTcpListener::from_std).transpose()?;

        let (admin, client) = admin::launch(repos, Arc::clone(&config), Arc::clone(&pool)).await?;

        if let Some(listener) = admin_listener {
            let addr = listener.local_addr()?;
            tokio::spawn(async move {
                tracing::info!(?addr, "Admin started");
                let app = axum::Router::new()
                    .route("/update/*rest", post(admin_update))
                    .with_state(admin);
                axum::serve(listener, app).await.expect("serves forever");
            });
        } else {
            tracing::debug!("Admin NOT started");
        }

        let app = Router::new()
            .route("/", get(handler))
            .route("/*rest", get(handler))
            .layer(TraceLayer::new_for_http())
            .layer(RequestBodyLimitLayer::new(0))
            .with_state(client);

        let addr = app_listener.local_addr()?;
        tracing::info!(?addr, "Server started");
        axum::serve(app_listener, app).await?;
        Ok(())
    })
}

fn discover_git_repos(basedir: PathBuf, export_all: bool) -> Result<Vec<PathBuf>> {
    let mut queue = vec![basedir];
    let mut candidates = Vec::new();

    'queue: while let Some(dir) = queue.pop() {
        let mut entries = std::fs::read_dir(&dir)?.flatten().collect::<Vec<_>>();

        // backwards so i can swap_remove safely
        for idx in (0..entries.len()).rev() {
            let entry = &entries[idx];
            let filetype = entry.file_type()?;
            if let Some(name) = entry.path().file_name() {
                if filetype.is_dir() && name == ".git" {
                    // dir contains a subdir named .git: likely a worktree
                    if !export_all && !dir.join(".git/git-daemon-export-ok").exists() {
                        tracing::debug!(?dir, ".git/git-daemon-export-ok not found");
                    } else {
                        candidates.push(dir);
                    }
                    continue 'queue;
                }
                if filetype.is_file() && name == "HEAD" {
                    // dir contains a HEAD file: likely a bare repo
                    if !export_all && !dir.join("git-daemon-export-ok").exists() {
                        tracing::debug!(?dir, "git-daemon-export-ok not found");
                    } else {
                        candidates.push(dir);
                    }
                    continue 'queue;
                }
            }

            // retain the subdirectories to keep searching
            if !filetype.is_dir() {
                entries.swap_remove(idx);
            }
        }

        // if `dir` is not a possible git repo, check its subdirectories
        queue.extend(entries.into_iter().map(|e| e.path().clone()));
    }

    Ok(candidates)
}

async fn admin_update(
    State(state): State<admin::SharedAdmin>,
    ReqPath(repo): ReqPath<String>,
) -> Response {
    let guard = state.admin.lock().await;
    match guard.update(repo).await {
        Ok(()) => StatusCode::OK.into_response(),
        Err(err) => match err {
            admin::UpdateError::Build(err) => match err {
                admin::BuildError::Urso(e) => {
                    (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response()
                }
                admin::BuildError::NotFound => StatusCode::NOT_FOUND.into_response(),
                admin::BuildError::PoolReceiveErr => (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "no response from pool during build",
                )
                    .into_response(),
            },
            admin::UpdateError::AdminDown => {
                (StatusCode::INTERNAL_SERVER_ERROR, "no response from admin").into_response()
            }
            admin::UpdateError::ClientDown => {
                (StatusCode::INTERNAL_SERVER_ERROR, "no response from client").into_response()
            }
        },
    }
}

fn open_repos(
    mut basedir: PathBuf,
    pool: &rayon::ThreadPool,
    config: Arc<GlobalConfig>,
) -> Result<Vec<repo::Repository>> {
    let git_dirs = discover_git_repos(basedir.clone(), config.export_all)?;

    // special case for when one uses a git repo as a basedir:
    // since basedir is used as a prefix, if it is a repository
    // it'd end up with an empty name. so replace it with its
    // parent
    if git_dirs.len() == 1 && Path::new(&basedir) == git_dirs[0].as_path() {
        let parent = &git_dirs[0].parent().ok_or("is / a git repo? lel")?;
        tracing::warn!("basedir may be git repo. will use: {parent:?} as base");
        basedir = parent.to_path_buf();
    }

    let prefix = format!("{}/", basedir.display());
    let (sender, receiver) = std::sync::mpsc::channel();
    let mut num_dirs = 0;
    for dir in git_dirs {
        num_dirs += 1;
        let sender = sender.clone();
        let config = Arc::clone(&config);

        let name = dir
            .to_string_lossy()
            .strip_prefix(&prefix)
            .map(|n| n.to_owned())
            .ok_or_else(|| format!("found repo at {dir:?} not prefix of {prefix}"))?;

        assert!(
            !name.is_empty(),
            "repo name must not be empty. prefix={prefix}"
        );

        pool.spawn(move || {
            // Yield back a name, the opened repository and its state
            // In case of errors it yields a name and which error happened
            let to_send = match urso::Urso::open(
                dir,
                config.max_file_size_bytes,
                config.rename_similarity_threshold,
                config.repo_object_cache_size,
            )
            .and_then(|urso| {
                RepoState::new(name.clone(), &urso, &config).map(|state| (urso, state))
            }) {
                Ok((urso, state)) => (name, Ok((urso.into_handle(), state))),
                Err(err) => (name, Err(err)),
            };

            sender.send(to_send).expect("can send reply");
        });
    }

    drop(sender);
    let mut repos = Vec::with_capacity(num_dirs);
    while let Ok((name, open_result)) = receiver.recv() {
        match open_result {
            Ok((handle, state)) => {
                tracing::debug!(
                    name,
                    head = state.snapshot.head.commit.message.title,
                    "Repository loaded"
                );
                repos.push(repo::Repository {
                    handle,
                    state: Arc::new(state),
                });
            }
            Err(err) => {
                tracing::error!("Error loading repo {name}: {err}");
            }
        }
    }

    // just so there's some stability
    repos.sort_unstable_by(|a, b| a.name.cmp(&b.name));
    Ok(repos)
}

async fn handler(State(client): State<Client>, uri: Uri) -> Response {
    client.handle(uri).await
}
