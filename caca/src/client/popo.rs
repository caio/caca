use std::{num::NonZeroUsize, sync::Arc};

use lru::LruCache;
use rayon::ThreadPool;
use tokio::sync::{mpsc, oneshot};

pub trait Command: PartialEq + std::hash::Hash {
    type Output;

    fn exec(self) -> Self::Output;
}

pub(crate) async fn launch<C, O>(pool: Arc<ThreadPool>, cache_size: NonZeroUsize) -> Popo<C, O>
where
    C: 'static + Clone + std::fmt::Debug + Send + Eq + std::hash::Hash + Command<Output = O>,
    O: 'static + Clone + std::fmt::Debug + Send,
{
    // The channel receives data from user input via the
    // Popo and from the threadpool when it completes
    // commands.
    let (sender, mut receiver) = mpsc::unbounded_channel::<Message<C, O>>();

    let mut state = State {
        registry: Registry::new(),
        cache: LruCache::new(cache_size),
        loopback: sender.clone(),
        pool,
    };

    tokio::spawn(async move {
        loop {
            while let Some(msg) = receiver.recv().await {
                match msg {
                    Message::Dispatch(cmd, result_tx) => {
                        state.dispatch(cmd, result_tx);
                    }
                    Message::Complete(id, res) => {
                        state.complete(id, res);
                    }
                };
            }
        }
    });

    Popo { sender }
}

#[derive(Debug)]
enum Message<Command, Output> {
    // emitted by the user via Deduper
    Dispatch(Command, oneshot::Sender<Output>),
    // emitted by the pool
    Complete(Id, Output),
}

#[derive(Debug, Clone)]
pub(crate) struct Popo<C, O> {
    sender: mpsc::UnboundedSender<Message<C, O>>,
}

#[derive(Debug, Clone)]
pub(crate) enum Error {
    PoolClosed,
    ResponseClosed,
}

impl<C, O> Popo<C, O> {
    pub async fn execute(&self, cmd: C) -> Result<O, Error> {
        let (result_tx, result_rx) = oneshot::channel();
        self.send(Message::Dispatch(cmd, result_tx)).await?;
        self.receive(result_rx).await
    }

    async fn send(&self, msg: Message<C, O>) -> Result<(), Error> {
        self.sender
            .send(msg)
            .map_err(|_discarded| Error::PoolClosed)
    }

    async fn receive<T>(&self, result_rx: oneshot::Receiver<T>) -> Result<T, Error> {
        result_rx.await.map_err(|_discarded| Error::ResponseClosed)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Id(u32);

struct Registry<K, V> {
    inner: Vec<Entry<K, V>>,
    next_id: u32,
}

#[derive(PartialEq, Debug, Clone)]
struct Entry<K, V> {
    id: Id,
    key: K,
    value: V,
}

impl<K: PartialEq, V> Registry<K, V> {
    fn new() -> Self {
        Self {
            inner: Vec::default(),
            next_id: 0,
        }
    }

    fn publish(&mut self, key: K, value: V) -> Id {
        debug_assert!(self.inner.iter().all(|i| i.key != key));
        let id = Id(self.next_id);
        self.next_id += self.next_id.wrapping_add(1);
        self.inner.push(Entry { key, id, value });
        id
    }

    fn unpublish(&mut self, id: Id) -> Option<(K, V)> {
        self.inner.iter().position(|i| i.id == id).map(|pos| {
            let entry = self.inner.swap_remove(pos);
            (entry.key, entry.value)
        })
    }
}

struct State<K, V> {
    registry: Registry<K, oneshot::Sender<V>>,
    cache: LruCache<K, V>,
    loopback: mpsc::UnboundedSender<Message<K, V>>,
    pool: Arc<rayon::ThreadPool>,
}

impl<C, O> State<C, O>
where
    C: 'static + Clone + std::fmt::Debug + Send + Eq + std::hash::Hash + Command<Output = O>,
    O: 'static + Clone + std::fmt::Debug + Send,
{
    fn complete(&mut self, id: Id, res: O) {
        if let Some((cmd, dst)) = self.registry.unpublish(id) {
            let _ignored = dst.send(res.clone());
            self.cache.put(cmd, res);
        }
    }

    fn dispatch(&mut self, cmd: C, dst: oneshot::Sender<O>) {
        if let Some(cached) = self.cache.get(&cmd) {
            let _ignored = dst.send(cached.clone());
            return;
        }

        // Otherwise, publish it with a single subscriber
        let id = self.registry.publish(cmd.clone(), dst);
        // And submit the command to the pool
        let sender = self.loopback.clone();
        self.pool.spawn(move || {
            // XXX on panic this ends up with a dangling enqueued
            //     command and one or more subscribers...
            //     ok since this pool propagates panics?
            let result = cmd.exec();
            // this is pool worker -> pool manager
            // can only fail if the manager goes away
            let _ignored_err = sender.send(Message::Complete(id, result));
        });
    }
}
