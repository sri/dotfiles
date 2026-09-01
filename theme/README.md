# Theme registry

`themes.json` is the source of truth for theme selection across Ghostty,
Kitty, Herdr, and Emacs. Each canonical theme records its light/dark
appearance and the spelling used by each application.

```sh
,theme list
,theme discover tokyo-night
,theme set tokyo-night
,theme status
,theme doctor
```

`,theme NAME` is shorthand for `,theme set NAME`.

`discover` queries installed Ghostty, Kitty, and Emacs themes, ranks names
after normalizing case, separators, and provider prefixes such as `doom-` and
`base16-`, and offers Herdr's built-in themes. Run it interactively to choose
and save a mapping. Variants such as Day, Moon, Night, and Storm remain
separate choices. When a theme does not exist in every application, map it to
the closest palette; Herdr's `terminal` theme inherits the terminal palette.

`set` validates every mapped provider, updates the registry and application
configs atomically, then reloads Ghostty, Kitty, Herdr, and a running Emacs.
`kitty/current-theme.conf` is generated from Kitty's official theme cache and
included by `kitty.conf`. Emacs reads the registry during startup and performs
a clean theme transition to prevent stale faces from previously loaded themes.
