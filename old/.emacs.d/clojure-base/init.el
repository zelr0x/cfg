;;; Package management
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "https://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(package-initialize)

;; GPG
;; If want to temporarily disable GPG verification: (setq package-check-signature nil)
;; And then enable it again: (setq package-check-signature 'allow-unsigned)
;; To update keys: (package-install 'gnu-elpa-keyring-update)
;; To solve GPG no key error, run: gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
;; If keyserver error, run the following instead (option-command order is important!):
;; gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver 'hkp://keyserver.ubuntu.com:11371' --receive-keys 066DAFCB81E42C40
;; If this particular keyserver errors out - find another one on the Web.
;; Install gpg with chocolatey: choco install gnupg

;; Fix GPG two concatenated home paths on Windows.
(if (eq system-type 'windows-nt)
    (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))

;; Update package metadata if needed.
;; Sometimes it doesn't work and have to be called manually for some reason (todo: research).
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    projectile
    color-theme-sanityinc-tomorrow))

;; On OS X, an Emacs instance started from the GUI has different env than a shell
;; in a terminal window, because OS X does not run a shell during the login.
;; This leads to unexpected results when calling external utils like make from Emacs.
;; This lib fixes it by copying important env vars from the shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

;; Install packages.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure

;;; UI
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Start maximazied.
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Side margins for caret symbols etc.
(set-fringe-mode 10)

;; Font.
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)

;; Electric RET indenting stuff.
;; Disable.
;(add-hook 'emacs-startup-hook
;          (lambda() (local-set-key (kbd "<RET>") 'electric-indent-just-newline)))
(defun electric-indent-mode-configure ()
  "Delete newline (?\n) from `electric-indent-chars'."
  (setq electric-indent-chars (delq 10 electric-indent-chars)))
(add-hook 'emacs-lisp-mode-hook #'electric-indent-mode-configure)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages '(gnu-elpa-keyring-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Color themes (color schemes).
;; Must be below `custom-set-variables` because it contains a list of safe themes.
;(load-theme 'tango-dark)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-night)
