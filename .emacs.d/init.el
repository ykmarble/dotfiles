;;; ======
;;;  base
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
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; change history length
(setq history-length 10000)

;;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; disable dialog box
(setq use-dialog-box nil)

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

;;; don't make backup file
(setq backup-inhibited t)

;;; delete auto save files
(setq delete-auto-save-files t)

;;; change tab width and indent width
(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "stroustrup")
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
		    :family "Ricty" ;;font
		    :height 160) ;;font-size
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty")) ;; font

;;; enable full screen mode with startup
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;; show line number
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%4d: ")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;;; set color theme
(load-theme 'tsdh-dark t)

;;; change bg-color
(set-face-background 'default "grey10")
(set-face-background 'hl-line "grey20")

;;; use transparent frame
(set-frame-parameter nil 'alpha 67)

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
    :foreground "SkyBlue1" :background "grey20"
    :inverse-video nil
    :box '(:line-width 2 :color "grey20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "SkyBlue1" :background "grey20"
    :inverse-video nil
    :box '(:line-width 2 :color "grey20" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :height 110
    :box '(:line-width 2 :color "grey20" :style nil))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :height 110
    :box '(:line-width 2 :color "grey20" :style nil))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00" :height 110)
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#7fff00"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :height 110)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "grey80"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "SkyBlue1")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")


;;; =======
;;;  utils
;;; =======

;;; use advanced occur
(require 'color-moccur)
(require 'moccur-edit)

;;; enable spell checker
(require 'ispell)
(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'yatex-mode-hook 'flyspell-mode)
(add-hook 'sgml-mode-hook '(lambda () (flyspell-mode -1)))

;;; migemo
(when (and (executable-find "cmigemo")
	   (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))  
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  (require 'helm-migemo)
  (setq helm-use-migemo t))

;;; flymake
(require 'flymake)
(require 'flymake-cursor)
(add-hook 'find-file-hook 'flymake-find-file-hook)
;(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;  (setq flymake-check-was-interrupted t))
;(ad-activate 'flymake-post-syntax-check)
;; function to do flymake without Makefile easily
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))
;; Arduino
(defun flymake-ino-init ()
 (flymake-simple-generic-init
  "/Applications/Arduino.app/Contents/Resources/Java/hardware/tools/avr/bin/avr-g++"
  '("-x" "c++" "-mmcu=atmega328p" "-I" (expand-file-name "/Users/marble/Source/arduino/include")
    "-include" "Arduino.h" "-Wall" "-Wextra" "-fsyntax-only")))
(push '("\\.ino\\'" flymake-ino-init) flymake-allowed-file-name-masks)
;; C/C++
(defun flymake-c-init ()
 (flymake-simple-generic-init
  "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
(defun flymake-cc-init ()
 (flymake-simple-generic-init
  "g++" '("-Wall" "-Wextra" "-fsyntax-only")))
(push '("\\.c\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\(cc\\|h\\|cpp\\|C\\|CPP\\|hpp\\)\\'" flymake-cc-init)
      flymake-allowed-file-name-masks)
;; Java
(defun flymake-java-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   buffer-file-name
   'flymake-get-java-cmdline))
(defun flymake-get-java-cmdline (source ase-dir)
  (list "javac" (list "-J-Dfile.encoding=utf-8" "-encoding" "utf-8"
              source)))
(push '("\\.java\\'" flymake-java-init) flymake-allowed-file-name-masks)
(add-hook 'java-mode-hook '(lambda () (flymake-mode t)))
;; Python
;(defun flymake-python-init ()
;  (flymake-simple-generic-init
;   "pyflakes" '()))
;(push '("\\.py\\'" flymake-python-init) flymake-allowed-file-name-masks)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq flymake-gui-warnings-enabled nil) ;; disable error dialog

;;; ==============
;;;  global modes 
;;; ==============

;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdonw files" t)
(setq auto-mode-alist
      (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq markdown-command-needs-filename t)

;;; Scala
(require 'scala-mode2)
(add-hook 'scala-mode-hook '(lambda ()
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
))

;;; Clojure
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;; YaTeX
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-kanji-code 4)
(setq tex-command "platex")
(setq dviprint-command-format "dvipdfmx %s")
(setq dvi2-command "open -a Preview")
(setq YaTeX-use-AMS-LaTeX t)
(setq bibtex-command "pbibtex")
(add-hook 'yatex-mode-hook '(lambda ()
  (define-key YaTeX-mode-map (kbd "C-c C-p") '(lambda () 
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
(if window-system (require 'caml-font))
(setq inferior-caml-program "/usr/local/bin/ocaml")


;;; SGML + Zen-coding
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook '(lambda () (setq emmet-indentation 2)))
(define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-line)
(eval-after-load "emmet-mode"  
  (define-key emmet-mode-keymap (kbd "C-j") nil))

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
(custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

;;; Auto complete
(require 'auto-complete-config)
(require 'auto-complete-clang)
(global-auto-complete-mode)
(setq-default ac-auto-show-menu nil)
(setq-default ac-ignore-case 'smart)
(setq-default ac-use-menu-map t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq-default ac-sources
  '(ac-source-dictionary ac-source-imenu ac-source-words-in-same-mode-buffers))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-semantic ac-source-semantic-raw) ac-sources)))
(defun my-ac-elisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-symbols ac-source-variables) ac-sources)))
(add-hook 'c-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
(add-hook 'arduino-mode-hook 'my-ac-cc-mode-setup)
(add-to-list 'ac-modes 'arduino-mode)
(add-hook 'emacs-lisp-mode-hook 'my-ac-elisp-mode-setup)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'python-2-mode)


;;; ======== 
;;;  popwin
;;; ========

(require 'popwin)
(require 'popwin-yatex)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 20)
(push '("*Completions*") popwin:special-display-config)
(push '("*Backtrace*") popwin:special-display-config)
(push '("*Warnings*") popwin:special-display-config)
(push '("*YaTeX-typesetting*") popwin:special-display-config)
(push '("*dvi-peview*") popwin:special-display-config)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)

;;; ==============
;;;  key bindings
;;; ==============

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-q") 'query-replace)
(global-set-key (kbd "C-o") 'moccur)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-t") 'helm-etags-select)
(global-set-key (kbd "M-o") 'helm-occur)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-s") 'helm-regexp)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x f") 'helm-recentf)
