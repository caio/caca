use axum::{
    http::{StatusCode, Uri},
    response::{IntoResponse, Response},
};
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot};

use crate::{
    repo::{RepoState, Repos},
    GlobalConfig,
};

mod handler;
mod popo;

pub(crate) async fn launch(
    config: Arc<GlobalConfig>,
    state: State,
    pool: Arc<rayon::ThreadPool>,
) -> Client {
    let popo = popo::launch(pool, config.cache_size).await;
    let reverse_proxy_base = config
        .site
        .reverse_proxy_base
        .as_ref()
        .cloned()
        .unwrap_or_default();
    let mut handler = handler::Handler {
        state,
        config,
        popo,
        reverse_proxy_base,
    };
    let (sender, mut receiver) = mpsc::unbounded_channel();
    let client = Client { sender };
    tokio::spawn(async move {
        while let Some(msg) = receiver.recv().await {
            match msg {
                Message::Handle(uri, dst) => {
                    if dst.send(handler.handle(uri).await).is_err() {
                        tracing::debug!("requester disconnected before response");
                    }
                }
                Message::CatchUp(params, dst) => {
                    let _ignored = dst.send(handler.catch_up(params));
                }
                Message::Reload(tmpl, dst) => {
                    handler.state.env.remove_template(&tmpl);
                    let _ignored = dst.send(handler.state.env.get_template(&tmpl).map(|_| ()));
                }
            }
        }
    });

    client
}

pub(crate) struct State {
    pub repos: Repos,
    pub env: minijinja::Environment<'static>,
}

enum Message {
    Handle(Uri, oneshot::Sender<Response>),
    CatchUp(Arc<RepoState>, oneshot::Sender<bool>),
    Reload(String, oneshot::Sender<Result<(), minijinja::Error>>),
}

#[derive(Clone, Debug)]
pub(crate) struct Client {
    sender: mpsc::UnboundedSender<Message>,
}

impl Client {
    pub(crate) async fn handle(&self, uri: Uri) -> Response {
        let (sender, receiver) = oneshot::channel();
        if self.sender.send(Message::Handle(uri, sender)).is_err() {
            return (StatusCode::INTERNAL_SERVER_ERROR, "client is closed").into_response();
        }
        match receiver.await {
            Ok(output) => output,
            Err(_) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                "client crashed while handling request",
            )
                .into_response(),
        }
    }

    pub(crate) async fn catchup(&self, params: Arc<RepoState>) -> bool {
        let (sender, receiver) = oneshot::channel();
        let _ignored = self.sender.send(Message::CatchUp(params, sender));
        receiver.await.unwrap_or(false)
    }

    pub(crate) async fn reload_template(
        &self,
        filename: String,
        dst: oneshot::Sender<Result<(), minijinja::Error>>,
    ) -> bool {
        self.sender.send(Message::Reload(filename, dst)).is_ok()
    }
}
