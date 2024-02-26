use std::sync::Arc;

use notify::{RecommendedWatcher, Watcher};
use tokio::sync::{
    mpsc::{self, UnboundedSender},
    oneshot, Mutex,
};

use crate::{
    client::{Client, State as ClientState},
    repo::{RepoState, Repos, Repository},
    GlobalConfig,
};

pub(crate) type SharedAdmin = Arc<AdminState>;

pub(crate) async fn launch(
    repos: Vec<Repository>,
    config: Arc<GlobalConfig>,
    pool: Arc<rayon::ThreadPool>,
) -> Result<(SharedAdmin, Client), Box<dyn std::error::Error>> {
    let (state, client_state) = prepare_state(
        repos,
        config.theme.env()?,
        Arc::clone(&config),
        Arc::clone(&pool),
    );

    let client = crate::client::launch(config, client_state, pool).await;
    let admin = launch_admin(state, client.clone()).await?;

    let shared_admin = Arc::new(AdminState {
        admin: Mutex::new(admin),
        _client: client.clone(),
    });

    Ok((shared_admin, client))
}

fn prepare_state(
    repos: Vec<Repository>,
    env: minijinja::Environment<'static>,
    config: Arc<GlobalConfig>,
    pool: Arc<rayon::ThreadPool>,
) -> (State, ClientState) {
    let client_state = ClientState {
        repos: Repos::new(&config, repos.clone()),
        env,
    };
    let state = State::new(repos, config, pool);

    (state, client_state)
}

async fn launch_admin(
    mut state: State,
    client: Client,
) -> Result<Admin, Box<dyn std::error::Error>> {
    let (sender, mut receiver) = mpsc::unbounded_channel();

    let watcher = launch_watcher(&state.repos, &state.config, sender.clone()).await?;

    let admin = Admin {
        sender,
        _watcher: watcher,
    };
    tokio::spawn(async move {
        while let Some(msg) = receiver.recv().await {
            handle_message(msg, &mut state, &client).await;
        }
    });

    Ok(admin)
}

async fn handle_message(msg: Message, state: &mut State, client: &Client) {
    match msg {
        Message::Update(name, dst) => {
            tracing::trace!(name, "received update request");
            match state.rebuild_snapshot(&name).await {
                Ok(repo) => {
                    tracing::debug!(
                        name,
                        head = ?repo.snapshot.head,
                        "admin has new state"
                    );
                    if client.catchup(repo).await {
                        tracing::trace!("client caught up");
                        let _ignored = dst.send(Ok(()));
                    } else {
                        tracing::error!("no confirmation from client");
                        let _ignored = dst.send(Err(UpdateError::ClientDown));
                    }
                }
                Err(e) => {
                    let _ignored = dst.send(Err(UpdateError::Build(e)));
                }
            };
        }
        Message::Reload(tmpl, dst) => {
            tracing::trace!(tmpl, "forwarding reload request to client");
            if !client.reload_template(tmpl, dst).await {
                tracing::error!("client looks down");
            }
        }
    }
}

enum Message {
    Update(String, oneshot::Sender<Result<(), UpdateError>>),
    Reload(String, oneshot::Sender<Result<(), minijinja::Error>>),
}

struct State {
    repos: Vec<Repository>,
    pool: Arc<rayon::ThreadPool>,
    config: Arc<GlobalConfig>,
}

impl State {
    fn new(
        repos: Vec<Repository>,
        config: Arc<GlobalConfig>,
        pool: Arc<rayon::ThreadPool>,
    ) -> Self {
        Self {
            repos,
            pool,
            config,
        }
    }

    async fn rebuild_snapshot(&mut self, name: &str) -> Result<Arc<RepoState>, BuildError> {
        let Some(pos) = self.repos.iter().position(|r| r.name == name) else {
            return Err(BuildError::NotFound);
        };

        let (sender, receiver) = oneshot::channel();
        let handle = self.repos[pos].handle.clone();

        let config = Arc::clone(&self.config);

        let name = name.to_string();
        self.pool.spawn(move || {
            let urso = handle.into_urso();
            let _ignored = sender.send(RepoState::new(name, &urso, &config));
        });

        let new_state = Arc::new(
            receiver
                .await
                .map_err(|_discarded| BuildError::PoolReceiveErr)??,
        );

        self.repos[pos].state = Arc::clone(&new_state);
        Ok(new_state)
    }
}

pub(crate) struct AdminState {
    pub(crate) admin: Mutex<Admin>,
    _client: Client,
}

pub(crate) struct Admin {
    sender: mpsc::UnboundedSender<Message>,
    _watcher: notify::RecommendedWatcher,
}

impl Admin {
    pub(crate) async fn update(&self, name: String) -> Result<(), UpdateError> {
        let (sender, receiver) = oneshot::channel();
        let _ignored = self.sender.send(Message::Update(name, sender));
        receiver
            .await
            .map_err(|_discarded| UpdateError::AdminDown)?
    }
}

#[derive(Debug)]
pub(crate) enum BuildError {
    Urso(urso::Error),
    NotFound,
    PoolReceiveErr,
}

impl From<urso::Error> for BuildError {
    fn from(value: urso::Error) -> Self {
        Self::Urso(value)
    }
}

#[derive(Debug)]
pub(crate) enum UpdateError {
    Build(BuildError),
    AdminDown,
    ClientDown,
}

async fn launch_watcher(
    repos: &[Repository],
    config: &GlobalConfig,
    admin: UnboundedSender<Message>,
) -> Result<RecommendedWatcher, Box<dyn std::error::Error>> {
    let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel();
    let watcher = spawn_sync_watcher(repos, sender, config)?;

    tokio::task::spawn(async move {
        let mut debounced = Vec::new();
        loop {
            // If there are enqueued events, wake up early to dispath
            // otherwise, wait forever for new events
            let timeout = if debounced.is_empty() {
                tokio::time::Duration::MAX
            } else {
                tokio::time::Duration::from_millis(500)
            };
            match tokio::time::timeout(timeout, receiver.recv()).await {
                Ok(Some(event)) => {
                    tracing::trace!(?event, "received update request from watcher");
                    match event {
                        WatcherEvent::Update(name) => {
                            let (tx, rx) = oneshot::channel();
                            admin.send(Message::Update(name, tx)).expect("admin works");
                            match rx.await {
                                Ok(_) => tracing::trace!("reload success"),
                                Err(err) => tracing::error!(?err, "failure reloading template"),
                            };
                        }
                        WatcherEvent::ReloadTemplate(tmpl) => {
                            if !debounced.iter().any(|d| d == &tmpl) {
                                debounced.push(tmpl);
                            }
                        }
                    }
                }
                Ok(None) => {
                    tracing::error!("watcher sender closed. shutting down");
                    break;
                }
                Err(_timeout) => {
                    for tmpl in debounced.drain(..) {
                        tracing::debug!(tmpl, "reloading template");
                        let (tx, rx) = oneshot::channel();
                        let _ignored = admin.send(Message::Reload(tmpl, tx));
                        match rx.await {
                            Ok(_) => tracing::trace!("reload success"),
                            Err(err) => tracing::error!(?err, "failure reloading template"),
                        };
                    }
                }
            }
        }
    });

    Ok(watcher)
}

fn spawn_sync_watcher(
    repos: &[Repository],
    sender: UnboundedSender<WatcherEvent>,
    config: &GlobalConfig,
) -> Result<notify::INotifyWatcher, Box<dyn std::error::Error>> {
    let mut info_to_name = Vec::with_capacity(repos.len());
    for repo in repos {
        info_to_name.push((repo.handle.git_dir().join("info/refs"), repo.name.clone()));
    }

    let theme_dir = config.theme.dir()?;
    let watch_theme = config.theme.watch_files();
    let theme_dir_copy = theme_dir.clone();

    let mut watcher =
        notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
            // sigh
            let Ok(event) = res else {
                return;
            };

            // git update-server-info just does a file replace dance
            // it's very easy to detect the end of the dance: atomic
            // rename where it swaps the temporary with the dst
            // (don't really need to be precise here, tho: when the
            // hook gets called the server _already_ has the update)
            let dst = event.paths.get(1);
            if matches!(
                event.kind,
                notify::EventKind::Modify(notify::event::ModifyKind::Name(
                    notify::event::RenameMode::Both
                ))
            ) && dst.is_some_and(|p| p.ends_with("info/refs"))
            {
                debug_assert_eq!(2, event.paths.len());
                let path = dst.unwrap();
                if let Some((_, name)) = info_to_name.iter().find(|(inforefs, _)| inforefs == path)
                {
                    tracing::trace!(name, "detected change on repo");
                    let _ignored = sender.send(WatcherEvent::Update(name.clone()));
                }
                return;
            }

            if !watch_theme || theme_dir_copy.is_none() {
                return;
            }

            // for themes, anything can happen sinde idk what's being
            // used to edit the files
            // debouncing is more important here
            match event.kind {
                notify::EventKind::Create(notify::event::CreateKind::File)
                | notify::EventKind::Modify(notify::event::ModifyKind::Data(_)) => {}
                _ => return,
            };
            let Some(path) = event.paths.first() else {
                return;
            };

            if !path
                // path.ends_with() is deceptive AF eh
                .extension()
                .and_then(|n| n.to_str())
                .is_some_and(|n| n == "html")
            {
                return;
            }

            let dir = theme_dir_copy.as_ref().expect("checked for is_some");

            if let Ok(rest) = path.strip_prefix(dir) {
                tracing::trace!(path=?rest, "detected theme change");
                let _ignored = sender.send(WatcherEvent::ReloadTemplate(
                    rest.to_string_lossy().into_owned(),
                ));
            }
        })?;

    for repo in repos.iter() {
        watcher.watch(
            &repo.handle.git_dir().join("info"),
            notify::RecursiveMode::NonRecursive,
        )?;
    }
    if watch_theme && theme_dir.is_some() {
        watcher.watch(
            theme_dir.as_ref().unwrap(),
            notify::RecursiveMode::Recursive,
        )?;
    }

    Ok(watcher)
}

#[derive(Debug)]
enum WatcherEvent {
    Update(String),
    ReloadTemplate(String),
}
