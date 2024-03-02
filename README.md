# caio's code asylum

caca - web front end for git repositories

# why

It all started with me trying to understand how `git log -- somefile`
was picking commits and still feeling confused after reading
[the docs][docs] (search for "A more detailed explanation follows", it's
well written). So I picked up [gix][] to hack my own file history walker
and, well, here we are...

[docs]: https://www.git-scm.com/docs/git-log
[gix]: https://github.com/Byron/gitoxide

# usage

    caca [-c /path/to/config] /path/to/gitroot

The (optional) configuration maps a gitconfig/ini file to [a GlobalConfig
instance](caca/src/config.rs#L13). See [here](caca/src/config.rs#L448) how
one could look like

You can use the `RUST_LOG` environment variable to configure logging.
The cmdline I tend to use when hacking is something like:

    RUST_LOG=debug cargo watch --ignore '*.html' -x "run ."

# features

- Repository metadata (description, url, owner, etc) is now version
  controlled. It reads a `.config/caca.ini` file (git-config format)
  in the default repository branch and keeps that up-to-date
  (path and branch configurable)

- [.mailmap](https://git-scm.com/docs/gitmailmap) support. If you
  use urls instead of emails, whenever an author name is shown,
  it'll be a hyperlink. The web ui doesn't show e-mails

- Atom feeds:

  - There's a global one with activities from every repo

  - Each repository has a feed which lists most recent
    tags and commits (all branches)

- Special "www" view: render markdown files automatically, hyperlinks
  to "folder" resolve as `folder/index.md` then `folder/index.html`.
  Other targets are served as-is, with content-type guessed by the
  filename

- systemd socket activation support

# ideas

- git blame? I took a blind stab at it once, realised way too late that
  I was assuming a linear history. Most of it is done and behaves like
   `git annotate --first-parent path/to/file` but it isn't very good
  (I annotate by rebuilding the file from its very first version, its
  perf is worst-case-always - `urso::annotate`)... maybe I'll just
  expose that and call it "git lame"

- Pikchr? Graphviz? I like plantuml and mermaid but I'm not keen on
  spinning a server up

- Could extend the www/ view with more smartness: allow alternative
  templates? Let markdown make use of the front-matter?

- Syntax highlight? I don't care for it it when looking at patches, but
  for blobs it's sometimes nice. I just feel like this is the browser
  responsibility, not the server's, so I keep avoiding it
  
# warts

- It's not CGI

- You have to enable the default `$GITDIR/hooks/post-update` script
  for every repository in the server (or do something similar)

- Many assumptions about data being utf-8 encoded

- Doesn't support `.gitattributes`

- Doesn't serve archives or .patch files

- Doesn't support the "fancy" http clone

- Doesn't claim to be blazingly fast

- Doesn't make you "code 55% faster"

- Doesn't contain "git" in the name


# the code

There are 2 crates:

- `caca`, the web server: it accepts requests, manages the state,
  controls access to the thread pool and renders html

- `urso` is where I started: got rev walk working for any given path
  and kept adding features on top

When `caca` starts, it builds an in-memory snapshot of every repository
it finds by traversing a base directory (optionally filtering for
`git-daemon-export-ok`) and uses this information to answer most
simple requests (listing, main repository pages and feeds)

There's a single admin (`caca::admin`) actor that manages the snapshots
and whenever a change happens within a repository the actor regenerates
the snapshot and submits it to the client (`caca::client`)

The client is responsible for matching requests (is the repository name
correct? branch name valid?) and routing accordingly. It makes use of
the "business logic" within `caca::repo` to craft the responses

Repository changes are detected by relying on git's [post-update][pu]
hook being called: `git update-server-info` outputs a file that caca
can watch for changes (`$GIT_DIR/info/refs`). Alternatively, there's
a rudimentary admin web "api" that can be used to trigger manual
updates via http

[pu]: https://git-scm.com/docs/githooks#post-update

## alternatives

If this model is not to your liking: there are really good CGI-based
([cgit][], [cgit-pink][], [gitweb][]) and static files ([stagit][])
alternatives; And if you'd like a server, just not this one, I'm
aware of [gitiles][]: I never operated it, but it looks great and the
goals are quite similar to this one

[cgit]: https://git.zx2c4.com/cgit/about/
[cgit-pink]: https://git.causal.agency/cgit-pink/about/
[gitweb]: https://git-scm.com/docs/gitweb
[stagit]: https://codemadness.org/stagit.html
[gitiles]: https://gerrit.googlesource.com/gitiles/

# license

This software is licensed under the [European Union Public License
(EUPL) v. 1.2 only][EUPL-1.2]

[EUPL-1.2]: https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12 
