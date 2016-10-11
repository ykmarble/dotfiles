;;; ======
;;;  Base
;;; ======

(require 'cl-lib)

;;; change frequency of garbage collection
(setq gc-cons-threshold 33554432)  ; 32MB

;;; language setting
(set-language-environment "Japanese")
(setenv "LANG" "ja_JP.UTF-8")
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)

;;; load-path setting
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;; add package-archives
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "https://marmalade-repo.org/packages/"))
(package-initialize)

;;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; change history length
(setq history-length 10000)

;;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; disable dialog box
(setq use-dialog-box nil)

;;; kill beep sound
(setq ring-bell-function 'ignore)

;;; stop cursor blinking
(blink-cursor-mode 0)

;;; kill whole line when cursor is head of line and C-k is pressed
(setq kill-whole-line t)

;;; ignore case on completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; give executable flag to files which start with #!
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; append directory name to buffer name when they are duplicate
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; auto rescan imenu list
(require 'imenu)
(setq imenu-auto-rescan t)

;; create backup file in ~/.emacs.d/backup
(setq make-backup-files t)
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
    backup-directory-alist))
;; create auto-save file in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;;; visible tab and spaces
(require 'whitespace)
(setq whitespace-style '(face trailing tabs empty tab-mark))
(setq whitespace-display-mappings '((tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq whitespace-action '(auto-cleanup))
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-trailing nil
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-background 'whitespace-trailing nil)
(set-face-background 'whitespace-tab nil)
(set-face-background 'whitespace-empty nil)

;;; change tab width and indent width
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java") (other . "linux")))

;;; dired configuration
(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "a") 'dired-find-file)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

;;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; cua-mode: rectangle region
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;; PATH setting
(exec-path-from-shell-initialize)

;;; disable auto indent
(electric-indent-mode -1)

;;; tramp hangs up when using zsh
(with-eval-after-load 'tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-default-method "scp"))

;; change C-h to Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

;;; ======
;;;  face
;;; ======

(require 'nyan-mode)
(setq nyan-bar-length 10)
(setq nyan-wavy-trail t)
(setq nyan-animate-nyancat t)
(nyan-start-animation)

;;; disable startup screen
(setq inhibit-startup-screen t)

;;; delete *bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; change string of title bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;;; highlight pare of paren
(show-paren-mode 1)

;;; highlight editting line
(global-hl-line-mode)

;;; font setting
(defvar my-font-height-normal 90) ; buffer
(defvar my-font-height-small 80)  ; larger text of modeline
(defvar my-font-height-foot 70)   ; smaller text of modeline
(defvar my-font-family "Source Han Code JP")
(set-face-attribute 'default nil
                    :family my-font-family ;;font
                    :height my-font-height-normal ) ;;font-size
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family my-font-family)) ;; font


;;; enable full screen mode with startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; set color theme
(load-theme 'tsdh-dark t)

;;; change region color
(set-face-background 'region "SlateBlue3")

;;; change bg-color
(set-face-background 'default "#000000")
(set-face-background 'hl-line nil)
(set-face-underline 'hl-line t)

;;; use transparent frame
(set-frame-parameter nil 'alpha 68)

;;; helm hilight line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white"))))
 '(helm-selection ((t (:background "SlateBlue3" :distant-foreground "black"))))
 '(helm-selection-line ((t (:background "SlateBlue3" :distant-foreground "black"))))
 '(highlight ((t (:background "SlateBlue3" :distant-foreground "black"))))
 '(sp-pair-overlay-face ((t (:inherit default)))))

;; mode line
(set-face-font 'mode-line my-font-family)
(set-face-font 'mode-line-inactive my-font-family)
(set-face-font 'mode-line-buffer-id my-font-family)

;;; change color settings of diff-mode
(with-eval-after-load 'diff-mode
  (set-face-attribute 'diff-added-face nil
                      :foreground "green")
  (set-face-attribute 'diff-removed-face nil
                      :foreground "firebrick1")
  (set-face-background 'diff-added-face nil)
  (set-face-background 'diff-removed-face nil)
  (set-face-background 'diff-changed-face nil)
  (set-face-background 'diff-function-face nil)
  (set-face-background 'diff-file-header-face nil)
  (set-face-background 'diff-refine-change nil))

;;; emacs powerline: change the face of footer
(setq-default
 mode-line-format
 '(
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t (propertize "    " 'face 'mode-line-modified-face))))
   ; Position, including warning for 80 columns
   (:propertize "%4l," face mode-line-position-face)
   (:eval (propertize "%3c " 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   " "
   (:eval (list (nyan-create)))
   ; mode indicators: vc, recursive edit, major mode, minor modes
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   ))
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-80col-face)
(set-face-attribute 'mode-line nil
    :foreground "SkyBlue1" :background "grey10"
    :inverse-video nil
    :height my-font-height-small
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "SkyBlue1" :background "grey10"
    :inverse-video nil
    :height my-font-height-small
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :height my-font-height-foot
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :height my-font-height-foot
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00"
    :height my-font-height-foot)
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "grey80"
    :height my-font-height-foot)
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;;; =======
;;;  utils
;;; =======

;;; disable electric indent-mode
(electric-indent-mode -1)

;;; insert current time
(defun my/insert-current-time()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d]" (current-time))))

;;; use advanced occur
(require 'color-moccur)
;(require 'moccur-edit)

;;; ag: The silver searcher
(require 'ag)
(setq ag-highlight-search t)  ; 検索キーワードをハイライト
(setq ag-reuse-buffers t)     ; 検索用バッファを使い回す (検索ごとに新バッファを作らない)

;;; wgrep
(require 'wgrep)
(require 'wgrep-ag)
(add-hook 'ag-mode-hook
          (lambda ()
            (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
            (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
            (wgrep-ag-setup)))


;;; enable spell checker
(with-eval-after-load 'ispell
  (setq-default ispell-program-name "aspell"))

;;; enable anzu
(global-anzu-mode +1)

;;; highlight same symbol
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode t)))

;;; ediff settings
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;;; recentf
(require 'recentf-ext)
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1) ;; auto-save

;;; smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-create-new-buffer 'always)
(when (boundp 'confirm-nonexistent-file-or-buffer)
  (setq confirm-nonexistent-file-or-buffer nil))
(setq ido-ignore-buffers (append ido-ignore-buffers '("\\`\\*.*\\*")))
(setq ido-enable-flex-matching t)

;;; junk file
(autoload 'open-junk-file "open-junk-file" nil t)
(with-eval-after-load 'open-junk-file
  (setq open-junk-file-find-file-function 'find-file)
  (setq open-junk-file-format "~/Scraps/%Y-%m-%d-%H%M%S."))

;;; speedbar
(with-eval-after-load 'sr-speedbar
  (setq sr-speedbar-right-side nil))

;;; migemo
;;/(when (executable-find "cmigemo")
;;  (require 'migemo)
;;  (setq migemo-options '("-q" "--emacs"))
;;  (setq migemo-command "cmigemo")
;;  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
;;  (setq migemo-coding-system 'utf-8-unix)
;;  (load-library "migemo")
;;  (migemo-init))

;;; enable undo tree
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;;; move cursor to previous point
(require 'point-undo)

;;; display diff between current buffer and HEAD of git
(require 'git-gutter)
(global-git-gutter-mode 1)

;;; make insert pairs smarter
(require 'smartparens-config)
(set-face-foreground 'sp-pair-overlay-face nil)
(set-face-background 'sp-pair-overlay-face nil)
(smartparens-global-mode)

;;; ==============
;;;  global modes
;;; ==============
;;; C/C++
(cmake-ide-setup)
(with-eval-after-load 'company-c-headers
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/"))
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-c") 'compile))

(with-eval-after-load 'gdb
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

;;; Python
(with-eval-after-load 'python-mode
  (global-unset-key (kbd "C-c !"))
  (define-key python-mode-map (kbd "C-c !") 'ipython-switch)
  (define-key python-mode-map (kbd "C-c C-c") 'py-execute-buffer-ipython)
  ; try to automagically figure out indentation
  (setq py-smart-indentation t))

;;; Ruby
(with-eval-after-load 'ruby-mode
  (require 'ruby-end))
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "M-SPC") 'markdown-preview)
  (define-key markdown-mode-map (kbd "TAB") 'markdown-indent-line)
  (setq markdown-command-needs-filename t)
  (setq markdown-command "pandoc -s -f markdown_github+tex_math_dollars+tex_math_double_backslash --mathjax"))

;;; HTML
(with-eval-after-load 'html-mode
  (define-key html-mode-map (kbd "C-c C-c v") 'browse-url-of-file))

;;; Scala
(defun scala/enable-eldoc ()
  "Show error message or type name at point by Eldoc."
  (setq-local eldoc-documentation-function
              '(lambda ()
                 (when (ensime-connected-p)
                   (ensime-type-at-point nil))))
  (eldoc-mode +1))
(defun scala/completing-dot-company ()
  (cond (company-backend
         (company-complete-selection)
         (scala/completing-dot))
        (t
         (insert ".")
         (company-complete))))
(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))
(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
         (insert "."))
        ((eq ensime-completion-style 'company)
         (scala/completing-dot-company))
        ((eq ensime-completion-style 'auto-complete)
         (scala/completing-dot-ac))))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ensime-mode-hook 'scala/enable-eldoc)
(with-eval-after-load 'scala-mode2
  (setq imenu-generic-expression
        '(
          ("var" "\\(var +\\)\\([^(): ]+\\)" 2)
          ("val" "\\(val +\\)\\([^(): ]+\\)" 2)
          ("override def" "^[ \\t]*\\(override\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          ("implicit def" "^[ \\t]*\\(implicit\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          ("def" "^[ \\t]*\\(def +\\)\\([^(): ]+\\)" 2)
          ("trait" "\\(trait +\\)\\([^(): ]+\\)" 2)
          ("class" "^[ \\t]*\\(class +\\)\\([^(): ]+\\)" 2)
          ("case class" "^[ \\t]*\\(case class +\\)\\([^(): ]+\\)" 2)
          ("object" "\\(object +\\)\\([^(): ]+\\)" 2)
          ))
  (setq scala-indent:step 4)
  (define-key scala-mode-map (kbd ".") 'scala/completing-dot))


;;; Emacs Lisp
(add-hook 'emacs-lisp-mode 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

;;; CommonLisp
(with-eval-after-load 'slime
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-banner)))

;;; Clojure
(add-hook 'clojure-mode-hook 'cider-mode)
;; mini bufferに関数の引数を表示させる
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(with-eval-after-load 'cider-mode
  ;; 'C-x b' した時に *nrepl-connection* と *nrepl-server* のbufferを一覧に表示しない
  (setq nrepl-hide-special-buffers t)
  ;; RELPのbuffer名を 'project名:nREPLのport番号' と表示する
  ;; project名は project.clj で defproject した名前
  (setq nrepl-buffer-name-show-port t)
  ;; buffer listにnrepl関係のものを表示しない
  (setq nrepl-hide-special-buffers t))

;;; YaTeX
(add-to-list 'auto-mode-alist '("\\.tex\\'" . yatex-mode))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (setq YaTeX-no-begend-shortcut t)
             (setq YaTeX-kanji-code 4)
             (setq tex-command "uplatex")
             (setq dviprint-command-format "dvipdfmx %s")
             (setq dvi2-command "atril")
             (setq YaTeX-use-AMS-LaTeX t)
             (setq bibtex-command "pbibtex")
             (setq YaTeX-close-paren-always 'never)
             (auto-fill-mode -1)))

;;; OCaml
(with-eval-after-load 'caml-mode
  (if window-system (require 'caml-font)))
(with-eval-after-load 'inferior-caml-mode
    (setq inferior-caml-program "/usr/bin/ocaml"))

;;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;(with-eval-after-load 'haskell-mode
;  (require 'haskell-interactive-mode)
;  (require 'haskell-process)
;  (custom-set-variables
;    '(haskell-process-suggest-remove-import-lines t)
;    '(haskell-process-auto-import-loaded-modules t)
;    '(haskell-process-log t)))

;;; SGML + Zen-coding
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(with-eval-after-load 'emmet-mode
  (setq emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-line)
  (define-key emmet-mode-keymap (kbd "C-j") nil))

;;; =============
;;;  completions
;;; =============

;;; yasnippet
(yas-global-mode 1)
;; add my snippet directory
(add-to-list 'yas-snippet-dirs "~/.emacs.d/elisp/yasnippet/snippets")
;; insert snippet
(define-key yas-minor-mode-map (kbd "C-x y i") 'helm-yas-complete)
;; open make new snippet window
(define-key yas-minor-mode-map (kbd "C-x y n") 'yas-new-snippet)
;; use helm interface
;(defun my-yas/prompt (prompt choices &optional display-fn)
;  (let* ((names (loop for choice in choices
;                      collect (or (and display-fn (funcall display-fn choice))
;                                  choice)))
;         (selected (helm-other-buffer
;                    `(((name . ,(format "%s" prompt))
;                       (candidates . names)
;                       (action . (("Insert snippet" . (lambda (arg) arg))))))
;                    "*helm yas/prompt*")))
;    (if selected
;        (let ((n (position selected names :test 'equal)))
;          (nth n choices))
;      (signal 'quit "user quit!"))))
(with-eval-after-load 'clojure-mode
  (clojure-snippets-initialize))

;;; Auto complete
;(require 'auto-complete-config)
;(global-auto-complete-mode t)
;(setq-default ac-ignore-case 'smart)
;(setq ac-delay 0.1)
;(setq ac-candidate-limit 15)
;(setq ac-auto-start 5)
;(setq-default ac-sources
;  '(ac-source-yasnippet ac-source-imenu ac-source-words-in-same-mode-buffers))
;(defun my-ac-cc-mode-setup ()
;  (setq ac-sources (append ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(defun my-ac-elisp-mode-setup ()
;  (setq ac-sources (append ac-sources '(ac-source-features ac-source-functions ac-source-symbols ac-source-variables))))
;(add-hook 'c-mode-hook 'my-ac-cc-mode-setup)
;(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
;(add-hook 'arduino-mode-hook 'my-ac-cc-mode-setup)
;(add-hook 'python-mode-hook 'jedi:setup)
;(with-eval-after-load 'jedi
;  (setq jedi:complete-on-dot t))
;(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)
;(add-hook 'cider-mode-hook 'ac-nrepl-setup)
;(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
;(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
;(add-to-list 'ac-modes 'haskell-interactive-mode)

;;; Company mode
(require 'company)
(add-hook 'prog-mode-hook 'company-mode-on)
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)
(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")
(add-hook 'c-mode-common-hook 'irony-mode)
(with-eval-after-load 'irony
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'irony-eldoc))
(with-eval-after-load 'flycheck
  (flycheck-irony-setup))
(add-hook 'python-mode-hook 'jedi:setup)
(with-eval-after-load 'jedi
  (setq jedi:complete-on-dot t))
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-c-headers)

;;; ========
;;;  popwin
;;; ========

(require 'popwin)
(require 'popwin-yatex)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 15)
(setq popwin:adjust-other-windows t)
(push '("*Completions*") popwin:special-display-config)
(push '("*Backtrace*") popwin:special-display-config)
(push '("*Warnings*") popwin:special-display-config)
(push '("*YaTeX-typesetting*") popwin:special-display-config)
(push '("*dvi-peview*") popwin:special-display-config)
(push '("^\\*helm" :regexp t :position right :width 0.5) popwin:special-display-config)
(push '("*quickrun*") popwin:special-display-config)
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)
(defadvice YaTeX-showup-buffer
    (around popwin-yatex:YaTeX-showup-buffer (buffer &optional func select) activate)
  (popwin:display-buffer-1 buffer
                           :default-config-keywords `(:noselect ,(not select))
                           :if-config-not-found (lambda (buffer) ad-do-it)))
(push '("*YaTeX-typesetting*") popwin:special-display-config)
(push '("*ensime-update*") popwin:special-display-config)
(push '("*slime-apropos*") popwin:special-display-config)
(push '("*slime-macroexpansion*") popwin:special-display-config)
(push '("*slime-description*") popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push '("*slime-xref*") popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push '(slime-repl-mode) popwin:special-display-config)
(push '(slime-connection-list-mode) popwin:special-display-config)

;;; ==============
;;;  key bindings
;;; ==============

(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-q") 'anzu-query-replace)
(global-set-key (kbd "M-q") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c d") 'my/insert-current-time)
(global-set-key (kbd "C-c j") 'open-junk-file)
(global-set-key (kbd "C-o") 'ag)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-t") 'helm-etags-select)
(global-set-key (kbd "M-o") 'helm-occur)
(global-set-key (kbd "M-i") 'helm-imenu-anywhere)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s") 'helm-regexp)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "C-.") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-,") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "M-SPC") 'quickrun-with-arg)
(global-set-key (kbd "C-x C-x") nil)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "M-,") 'point-undo)
(global-set-key (kbd "M-.") 'point-redo)
(global-set-key (kbd "C-c SPC") 'gud-break)
(global-set-key (kbd "C-c .") 'company-complete)
(with-eval-after-load 'helm
  (define-key helm-map (kbd "M-h") 'backward-kill-word))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages
   (quote
    (clj-mode ipython pyflakes pymacs tbemail zencoding-mode yaml-mode wgrep-ag virtualenv undo-tree twittering-mode tango-2-theme swiper sr-speedbar smooth-scroll smartparens slime slamhound ruby-end ruby-block robe recentf-ext rbenv rainbow-mode rainbow-delimiters quickrun qml-mode python-mode powerline popwin point-undo paredit pallet open-junk-file oauth nyan-mode nose multiple-cursors multi-term matlab-mode markdown-mode+ main-line magit lua-mode jedi irony-eldoc imenus imenu-anywhere iedit idomenu highlight-symbol helm-migemo helm-flycheck helm-c-yasnippet google-translate git-gutter-fringe fuzzy flymake-ruby flymake-python-pyflakes flycheck-irony flycheck-ghcmod expand-region exec-path-from-shell evil-numbers esup ensime emmet-mode elpy direx company-jedi company-irony company-ghc company-c-headers color-theme-solarized color-moccur coffee-mode cmake-mode cmake-ide clojure-snippets clojure-cheatsheet caml auto-compile auctex arduino-mode anzu ag ac-haskell-process ac-cider))))
