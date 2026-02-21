Below is a practical `skills.md` tailored for **LLM-driven Emacs theming via `emacsclient -e`**.

---

# skills.md

## Emacs Theme Manipulation via `emacsclient -e`

### Purpose

Enable an LLM agent to:

* Inspect current theme + faces
* Compute complementary or derived colors
* Modify faces live
* Reload themes instantly
* Persist changes to config

All changes must be applied via:

```
emacsclient -e "(elisp-expression)"
```

---

## 1. Requirements

* Emacs running as server:

  ```elisp
  (server-start)
  ```
* `emacsclient` available in PATH
* `color.el` loaded (for color computations)

---

## 2. Core Capabilities

### 2.1 List Enabled Themes

```bash
emacsclient -e "custom-enabled-themes"
```

---

### 2.2 List All Faces

```bash
emacsclient -e "(face-list)"
```

---

### 2.3 Inspect Face Attributes

```bash
emacsclient -e "(face-attribute 'org-level-1 :foreground)"
```

Common attributes:

* `:foreground`
* `:background`
* `:weight`
* `:slant`
* `:underline`
* `:overline`
* `:box`

---

### 2.4 Set Face Attribute (Live)

```bash
emacsclient -e "(set-face-attribute 'org-level-2 nil :foreground \"#cb4b16\")"
```

Changes apply instantly.

---

### 2.5 Reload Current Theme

Soft reload:

```bash
emacsclient -e "(enable-theme 'solarized-dark)"
```

Hard reload:

```bash
emacsclient -e "(mapc #'disable-theme custom-enabled-themes)"
emacsclient -e "(load-theme 'solarized-dark t)"
```

---

## 3. Color Intelligence

### 3.1 Load Color Utilities

```bash
emacsclient -e "(require 'color)"
```

---

### 3.2 Compute Complementary Color

```bash
emacsclient -e "(color-complement \"#b58900\")"
```

---

### 3.3 Adjust Lightness

```bash
emacsclient -e "(color-lighten-name \"#b58900\" 10)"
```

Darken:

```bash
emacsclient -e "(color-darken-name \"#b58900\" 10)"
```

---

### 3.4 Compute Contrast Ratio (WCAG)

```bash
emacsclient -e "(color-contrast-ratio \"#ffffff\" \"#002b36\")"
```

Target:

* 4.5+ for normal text
* 7+ for strong readability

---

## 4. Example Agent Workflows

---

### Example 1: Complement org-level-1 → Apply to org-level-2

1. Get color:

   ```bash
   emacsclient -e "(face-attribute 'org-level-1 :foreground)"
   ```

2. Compute complement:

   ```bash
   emacsclient -e "(color-complement \"#b58900\")"
   ```

3. Apply:

   ```bash
   emacsclient -e "(set-face-attribute 'org-level-2 nil :foreground \"#268bd2\")"
   ```

---

### Example 2: Improve TODO Visibility in Solarized Dark

1. Get background:

   ```bash
   emacsclient -e "(face-attribute 'default :background)"
   ```

2. Compute high contrast candidate

3. Apply:

   ```bash
   emacsclient -e "(set-face-attribute 'org-todo nil :foreground \"#dc322f\" :weight 'bold)"
   ```

---

### Example 3: Add Top Border to Mode Line

```bash
emacsclient -e "(set-face-attribute 'mode-line nil :overline \"#586e75\")"
```

---

## 5. Persisting Changes

To save to init:

Append to config file:

```bash
emacsclient -e "(with-temp-buffer
  (insert \"(set-face-attribute 'org-level-2 nil :foreground \\\"#cb4b16\\\")\")
  (append-to-file (point-min) (point-max) \"~/.emacs.d/theme-overrides.el\"))"
```

Then load that file in init:

```elisp
(load \"~/.emacs.d/theme-overrides.el\")
```

---

## 6. Recommended Safe Guardrails for Agent

* Never eval arbitrary user Lisp
* Only allow:

  * `set-face-attribute`
  * `face-attribute`
  * `color-*`
  * `enable-theme`
  * `load-theme`
* Do not modify files outside config path
* Always verify contrast ratio before applying

---

## 7. Suggested Minimal Wrapper Script (Optional)

Instead of raw calls, create:

```
emacs-theme-tool eval "(...)"
```

That wraps:

```bash
emacsclient -e "$1"
```

This allows cleaner tool integration.

---

## 8. Live Feedback Model

Each mutation is:

1. Compute color
2. Apply face change
3. Return confirmation
4. User sees change instantly

No restart required.

---

## 9. Advanced Extensions (Optional)

* Generate full derived palette from base hue
* Normalize saturation levels
* Auto-balance org heading hierarchy
* Compute Solarized-consistent variants
* Snapshot + revert capability

---

## Summary

Using `emacsclient -e` enables:

* Direct live theme mutation
* Zero plugin required
* Immediate UI feedback
* Full programmable control
* Clean LLM integration

This approach is lightweight, composable, and production-safe for advanced Emacs theming automation.
