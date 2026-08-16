# Theme registry

`themes.json` is the source of truth for theme selection across Ghostty,
Herdr, and Emacs. Each canonical theme records the spelling used by each
application.

```sh
,theme list
,theme discover tokyo-night
,theme set tokyo-night
,theme status
,theme doctor
```

`,theme NAME` is shorthand for `,theme set NAME`.

`discover` queries installed Ghostty and Emacs themes, ranks names after
normalizing case, separators, and provider prefixes such as `doom-` and
`base16-`, and offers Herdr's built-in themes. Run it interactively to choose
and save a mapping. Variants such as Day, Moon, Night, and Storm remain
separate choices.

`set` validates every mapped provider, updates the registry and application
configs atomically, then reloads Ghostty, Herdr, and a running Emacs. Emacs
reads the registry during startup and performs a clean theme transition to
prevent stale faces from previously loaded themes.
