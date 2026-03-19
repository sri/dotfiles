;;; export-emacs-theme-packages.el --- Export recent theme packages -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)

(defun export--pkg-year-from-version (version)
  "Return year inferred from VERSION list, or nil if it cannot be inferred."
  (let ((vstr (package-version-join version)))
    (cond
     ;; MELPA style: YYYYMMDD(.N)
     ((string-match "\\`\\([0-9]\\{4\\}\\)[0-9]\\{4\\}\\(?:\\..*\\)?\\'" vstr)
      (string-to-number (match-string 1 vstr)))
     ;; Occasionally version begins with year-like major component, e.g. 2022.1
     ((string-match "\\`\\([0-9]\\{4\\}\\)\\(?:\\..*\\)?\\'" vstr)
      (string-to-number (match-string 1 vstr)))
     (t nil))))

(defun export--pkg-url (desc)
  "Extract URL from package DESC extras."
  (let ((extras (package-desc-extras desc)))
    (or (alist-get :url extras)
        (alist-get 'url extras)
        "")))

(defun export--normalize-desc (raw)
  "Return a single package-desc from RAW, or nil if none found.
RAW may be a package-desc, list of package-descs, or vector of package-descs."
  (cond
   ((package-desc-p raw) raw)
   ((listp raw) (cl-find-if #'package-desc-p raw))
   ((vectorp raw) (cl-find-if #'package-desc-p (append raw nil)))
   (t nil)))

(defun export--html-escape (s)
  "Basic HTML escaping for string S."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s t t))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (setq s (replace-regexp-in-string ">" "&gt;" s t t))
    (setq s (replace-regexp-in-string "\"" "&quot;" s t t))
    s))

(defun export-theme-packages ()
  "Refresh package metadata and export recent theme packages to dated .el and .html files."
  (let* ((cutoff-year (- (string-to-number (format-time-string "%Y")) 7))
         (outfile (format-time-string "%F-emacs-packages.el"))
         (htmlfile (format-time-string "%F-emacs-packages.html"))
         rows)
    ;; Include MELPA so theme packages are available in --batch -Q runs.
    (setq package-archives
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa" . "https://melpa.org/packages/")))

    (package-initialize)
    (package-refresh-contents)

    (dolist (entry package-archive-contents)
      (let* ((name (car entry))
             (desc (export--normalize-desc (cdr entry))))
        (when desc
          (let* ((name-str (symbol-name name))
                 (year (export--pkg-year-from-version (package-desc-version desc))))
            (when (and (string-match-p "theme" (downcase name-str))
                       year
                       (>= year cutoff-year))
              (push (list :name name-str
                          :url (export--pkg-url desc)
                          :description (or (package-desc-summary desc) ""))
                    rows))))))

    (setq rows (nreverse rows))

    (with-temp-file outfile
      (insert ";;; " outfile " --- Auto-generated theme package export -*- lexical-binding: t; -*-\n")
      (insert ";;; Generated: " (format-time-string "%F %T %z") "\n\n")
      (insert "(setq emacs-theme-packages\n      '")
      (prin1 rows (current-buffer))
      (insert ")\n"))

    (with-temp-file htmlfile
      (insert "<!doctype html>\n<html lang=\"en\">\n<head>\n")
      (insert "  <meta charset=\"utf-8\">\n")
      (insert "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n")
      (insert "  <title>Emacs Theme Packages</title>\n")
      (insert "  <style>\n")
      (insert "    :root{color-scheme:light dark;--bg:#0b1020;--card:#121a2f;--text:#e8ecff;--muted:#9da8c7;--accent:#7aa2ff;--border:#243154;}\n")
      (insert "    @media (prefers-color-scheme:light){:root{--bg:#f6f8ff;--card:#ffffff;--text:#1f2740;--muted:#5c678b;--accent:#315efb;--border:#dbe2ff;}}\n")
      (insert "    *{box-sizing:border-box} body{margin:0;font:15px/1.5 Inter,system-ui,-apple-system,Segoe UI,Roboto,sans-serif;background:var(--bg);color:var(--text)}\n")
      (insert "    .wrap{max-width:1100px;margin:40px auto;padding:0 20px} h1{font-size:clamp(1.4rem,2.2vw,2rem);margin:0 0 8px} .meta{color:var(--muted);margin-bottom:24px}\n")
      (insert "    .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:14px} .card{background:var(--card);border:1px solid var(--border);border-radius:14px;padding:14px;box-shadow:0 8px 24px rgba(0,0,0,.15)}\n")
      (insert "    .name{font-weight:650;margin:0 0 8px} .desc{color:var(--muted);margin:0 0 12px;min-height:42px} a{color:var(--accent);text-decoration:none} a:hover{text-decoration:underline}\n")
      (insert "  </style>\n</head>\n<body>\n  <div class=\"wrap\">\n")
      (insert "    <h1>Emacs Theme Packages</h1>\n")
      (insert (format "    <p class=\"meta\">Generated: %s • %d packages • versions in last 7 years</p>\n"
                      (format-time-string "%F %T %z") (length rows)))
      (insert "    <div class=\"grid\">\n")
      (dolist (row rows)
        (let ((name (export--html-escape (plist-get row :name)))
              (url (export--html-escape (plist-get row :url)))
              (desc (export--html-escape (plist-get row :description))))
          (insert "      <article class=\"card\">\n")
          (insert (format "        <h2 class=\"name\">%s</h2>\n" name))
          (insert (format "        <p class=\"desc\">%s</p>\n" desc))
          (if (and url (> (length url) 0))
              (insert (format "        <a href=\"%s\" target=\"_blank\" rel=\"noopener noreferrer\">Open project ↗</a>\n" url))
            (insert "        <span class=\"desc\">No URL</span>\n"))
          (insert "      </article>\n")))
      (insert "    </div>\n  </div>\n</body>\n</html>\n"))

    (princ (format "Wrote %s and %s with %d packages\n" outfile htmlfile (length rows)))))

(export-theme-packages)
