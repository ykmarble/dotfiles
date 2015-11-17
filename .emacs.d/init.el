;;; ======
;;;  Base
;;; ======

;;; change frequency of garbage collection
(setq gc-cons-threshold 33554432)  ;; 32MB

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
      "http://marmalade-repo.org/packages/"))
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
(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java") (other . "linux")))
(setq-default indent-tabs-mode)

;;; dired configuration
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; cua-mode: rectangle region
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;; PATH setting
(exec-path-from-shell-initialize)

;;; auto reload file which modified by other editor
;(auto-revert-mode t)

;;; disable auto indent
(electric-indent-mode -1)

;;; tramp hangs up when using zsh
(require 'tramp)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "scp")

;; change C-h to Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)


;;; move cursor backword when insert pair of parens
(defun my-insert-bracket-general (lbrackets rbracket)
  (insert rbracket)
  (when (memq (char-before (1- (point))) lbrackets)
    (backward-char)))
(defun my-insert-paren ()
  (interactive) (my-insert-bracket-general '(?\() ?\)))
(defun my-insert-brace ()
  (interactive) (my-insert-bracket-general '(?{) ?}))
(defun my-insert-bracket ()
  (interactive) (my-insert-bracket-general '(?\[) ?\]))
(defun my-insert-angle ()
  (interactive) (my-insert-bracket-general '(?<) ?>))
(defun my-insert-dquote ()
  (interactive) (my-insert-bracket-general '(?\") ?\"))
(defun my-insert-squote ()
  (interactive) (my-insert-bracket-general '(?' ?`) ?'))
(global-set-key "\)" 'my-insert-paren)
(global-set-key "}"  'my-insert-brace)
(global-set-key "\]" 'my-insert-bracket)
(global-set-key ">"  'my-insert-angle)
(global-set-key "\"" 'my-insert-dquote)
(global-set-key "'"  'my-insert-squote)


;;; ======
;;;  face
;;; ======

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
(set-face-attribute 'default nil
                    :family "Source Han Code JP" ;;font
                    :height 90
                    :weight 'bold) ;;font-size
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Source Han Code JP")) ;; font


;;; enable full screen mode with startup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(yas-prompt-functions (quote (my-yas/prompt))))

;;; show line number
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%4d: ")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;;; set color theme
(load-theme 'tsdh-dark t)

;;; change region color
(set-face-background 'region "SlateBlue3")

;;; change bg-color
(set-face-background 'default "#000000")
(set-face-background 'hl-line nil)
(set-face-underline-p 'hl-line t)
;;; use transparent frame
(set-frame-parameter nil 'alpha 68)

;;; helm hilight line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "SlateBlue3" :distant-foreground "black"))))
 '(helm-selection-line ((t (:background "SlateBlue3" :distant-foreground "black"))))
 '(highlight ((t (:background "SlateBlue3" :distant-foreground "black"))))
)


;; mode line
(set-face-font 'mode-line "Source Han Code JP")
(set-face-font 'mode-line-inactive "Source Han Code JP")
(set-face-font 'mode-line-buffer-id "Source Han Code JP")

;;; change color settings of diff-mode
(require 'diff-mode)
(set-face-attribute 'diff-added-face nil
                    :foreground "green")
(set-face-attribute 'diff-removed-face nil
                    :foreground "firebrick1")
(set-face-background 'diff-added-face nil)
(set-face-background 'diff-removed-face nil)
(set-face-background 'diff-changed-face nil)
(set-face-background 'diff-function-face nil)
(set-face-background 'diff-file-header-face nil)
(set-face-background 'diff-refine-change nil)


;;; emacs powerline: change the face of footer
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
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
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(set-face-attribute 'mode-line nil
    :foreground "SkyBlue1" :background "grey10"
    :inverse-video nil
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "SkyBlue1" :background "grey10"
    :inverse-video nil
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :height 60
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :height 60
    :box '(:line-width 2 :color "grey10" :style nil))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00"
    :height 75)
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :height 60)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "grey80"
    :height 60)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "SkyBlue1")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;;; =======
;;;  utils
;;; =======

;;; insert current time
(defun insert-current-time()
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
(add-hook 'ag-mode-hook '(lambda ()
                           (require 'wgrep-ag)
                           (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
                           (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
                           (wgrep-ag-setup)))


;;; enable spell checker
(autoload 'ispell "ispell" nil t)
(eval-after-load 'ispell '(setq-default ispell-program-name "aspell"))

;;; enable anzu
(global-anzu-mode +1)

;;; highlight same symbol
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode t)))

;;; ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; recentf
(when (require 'recentf-ext nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)) ;; auto-save

;;; magit
(require 'magit)
;(setq magit-diff-refine-hunk nil)
;(set-face-background 'magit-item-highlight nil) ; disable highlight
;(set-face-attribute 'magit-item-highlight nil :inherit nil)

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

;;; junk file
(require 'open-junk-file)
(setq open-junk-file-format "~/Scraps/%Y-%m-%d-%H%M%S.")

;;; quickrun
(require 'quickrun)

;;; speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)


;;; migemo
(when (and (executable-find "cmigemo")
       (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
;  (require 'helm-migemo)
;  (setq helm-use-migemo t)
  )

;;; enable undo tree
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; ==============
;;;  global modes
;;; ==============

;;; Python
(autoload 'python-mode "python-mode" nil t)
(eval-after-load 'python-mode
  '(progn
    (global-unset-key (kbd "C-c !"))
    (define-key python-mode-map (kbd "C-c !") 'ipython-switch)
    (define-key python-mode-map (kbd "C-c C-c") 'py-execute-buffer-ipython)))

; switch to the interpreter after executing code
;(setq py-shell-switch-buffers-on-execute-p t)
;(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

;;; Markdown
(setq auto-mode-alist
      (cons '("\\.md$" . gfm-mode) auto-mode-alist))
(setq markdown-command-needs-filename t)
(eval-after-load 'markdown-mode
   '(progn
     (define-key markdown-mode-map (kbd "M-SPC") 'markdown-preview)
     (define-key markdown-mode-map (kbd "TAB") 'markdown-indent-line)
     (setq markdown-command "pandoc -s -f markdown_github+tex_math_dollars+tex_math_double_backslash --mathjax")))

;;; HTML
(eval-after-load 'html-mode
  '(progn
     (define-key html-mode-map (kbd "C-c C-c v") 'browse-url-of-file)))

;;; Scala
(autoload 'scala-mode2 "scala-mode2" nil t)
(eval-after-load 'scala-mode2
  '(progn
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
     (require 'ensime)
     (ensime-scala-mode-hook)
     (setq scala-indent:step 4)
     (setq ensime-completion-style 'auto-complete)
     ))

;;; Clojure
(add-hook 'clojure-mode-hook 'cider-mode)
;; mini bufferに関数の引数を表示させる
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; 'C-x b' した時に *nrepl-connection* と *nrepl-server* のbufferを一覧に表示しない
(setq nrepl-hide-special-buffers t)
;; RELPのbuffer名を 'project名:nREPLのport番号' と表示する
;; project名は project.clj で defproject した名前
(setq nrepl-buffer-name-show-port t)
;; buffer listにnrepl関係のものを表示しない
(setq nrepl-hide-special-buffers t)

;;; YaTeX
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-hook 'yatex-mode-hook '(lambda ()
  (setq YaTeX-no-begend-shortcut t)
  (setq YaTeX-kanji-code 4)
  (setq tex-command "uplatex")
  (setq dviprint-command-format "dvipdfmx %s")
  (setq dvi2-command "atril")
  (setq YaTeX-use-AMS-LaTeX t)
  (setq bibtex-command "pbibtex")
  (setq YaTeX-close-paren-always nil)
  (auto-fill-mode -1)
  (define-key YaTeX-mode-map (kbd "M-SPC") '(lambda ()
    (interactive)
    (YaTeX-typeset-menu nil ?j)
    (YaTeX-typeset-menu nil ?d)
    (YaTeX-typeset-menu nil ?p)
  ))
))

;;; Arduino
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;; OCaml
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?\$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(add-hook 'caml-mode-hook '(lambda ()
    (if window-system (require 'caml-font))))
(add-hook 'inferior-caml-mode-hook '(lambda ()
    (setq inferior-caml-program "/usr/bin/ocaml")))

;;; Octave
(autoload 'octave-mode "octave-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(autoload 'run-octave "octave-inf" nil t)
(eval-after-load 'inferior-octave-mode '(setq inferior-octave-program "/usr/bin/octave"))

;;; Haskell
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(setq haskell-program-name "/usr/bin/ghci")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;; Coq
(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;;; SGML + Zen-coding
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook '(lambda () (setq emmet-indentation 2)))
(eval-after-load 'emmet-mode
  '(progn
     (define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-line)
     (define-key emmet-mode-keymap (kbd "C-j") nil)))


;;; =============
;;;  completions
;;; =============

;;; yasnippet
(yas-global-mode 1)
;; add my snippet directory
(setq yas-snippet-dirs
      (append yas-snippet-dirs '("~/.emacs.d/elisp/yasnippet/snippets")))
;; insert snippet
(define-key yas-minor-mode-map (kbd "C-x y i") 'yas-insert-snippet)
;; open make new snippet window
(define-key yas-minor-mode-map (kbd "C-x y n") 'yas-new-snippet)
;; use helm interface
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  coice)))
         (selected (helm-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*helm yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))

(autoload 'clojure-snippets "clojure-snipeets" nil t)
(clojure-snippets-initialize)

;;; Auto complete
(require 'auto-complete-config)
(autoload 'auto-complete-clang "auto-complete-clang" nil t)
(global-auto-complete-mode t)
(setq-default ac-ignore-case 'smart)
(setq ac-delay 0.1)
;;(global-set-key (kbd ".") (lambda () (interactive) (insert ".") (auto-complete)))
(setq ac-candidate-limit 15)
(setq ac-auto-start 5)
(setq-default ac-sources
  '(ac-source-yasnippet ac-source-imenu ac-source-words-in-same-mode-buffers))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-semantic ac-source-semantic-raw) ac-sources)))
(defun my-ac-elisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-symbols ac-source-variables) ac-sources)))
(add-hook 'c-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'arduino-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)
(autoload 'jedi "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(autoload 'ac-nrepl "ac-nrepl" nil t)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)

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
(global-set-key (kbd "C-c d") `insert-current-time)
(global-set-key (kbd "C-c j") 'open-junk-file)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c h") 'howm-menu)
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
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(require 'helm)
(define-key helm-map (kbd "M-h") 'backward-kill-word)
(put 'scroll-left 'disabled nil)
