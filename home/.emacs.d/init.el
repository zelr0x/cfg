;; Mostly copied from Jonathan Blow's compiler development streams
;; and from Casey Muratori's Handmade Hero streams.

(require 'compile)
;; Add syntax highlighting for batch files and stuff.
(require 'generic-x)
(require 'cc-mode)
;; Enables paths completion and some other stuff.
(require 'ido)
(ido-mode t)

;; Default encodings. In some contexts, 'utf-8-unix might be better.
;; M-x describe-coding-system for details.
;; Maybe replace all of this with (set-language-environment "UTF-8")?
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
;; Backwards compatibility layer for versions <= 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq default-buffer-file-coding-system 'utf-8-unix))

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(global-linum-mode t)
(setq column-number-mode t)

(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when (= (length (window-list)) 1)
  (split-window-horizontally))

(setq visible-bell t)
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs for indenting.
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

;; Always terminate last line in file.
(setq require-final-newline t)
;; Lines of overlap when scrolling.
(setq next-screen-context-lines 1)
;; Autosave every N characters typed
(setq auto-save-interval 300)
;; Autorevert buffers if files on disk change.
(global-auto-revert-mode 1)
(global-font-lock-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#d3b58d" :background "#041818"))))
 '(custom-group-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(custom-variable-tag-face ((t (:underline t :foreground "lightblue"))) t)
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "#48b751"))))
 ;'(font-lock-comment-face ((t (:foreground "#44b340"))))
 ;'(font-lock-comment-face ((t (:foreground "#3fdf1f"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 ;'(font-lock-string-face ((t (:foreground "gray160" :background "gray16"))))
 '(font-lock-string-face ((t (:foreground "#0fdfaf"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "#c8d4ec"))))
 ;'(font-lock-warning-face ((t (:foreground "#695a46"))))
 '(font-lock-warning-face ((t (:foreground "#504038"))))
 '(highlight ((t (:foreground "navyblue" :background "darkseagreen2"))))
 '(mode-line ((t (:inverse-video t))))
 '(region ((t (:background "blue"))))
 '(widget-field-face ((t (:foreground "white"))) t)
 '(widget-single-line-field-face ((t (:background "darkgray"))) t))

;(set-background-color "#072626")
;(set-background-color "white")
(set-cursor-color "lightgreen")

(set-face-attribute 'default nil :font "Consolas-12")
;; (set-face-attribute 'default nil :font "Monaco For Powerline-12")

(set-face-foreground 'font-lock-builtin-face "lightgreen")
;(set-face-foreground 'font-lock-constant-face "darkred")
;(set-face-foreground 'font-lock-function-name-face "cyan3")
;(set-face-foreground 'font-lock-string-face "green3")
;(set-face-foreground 'font-lock-type-face "dodgerblue")
;(set-face-foreground 'font-lock-variable-name-face "coral")

(if (not (boundp 'already-augmented-path))
    (setq load-path
      (append load-path (list "/bin")))
)
(setq already-augmented-path t)

(defun compile-helper (key-sequence)
  (save-some-buffers "!") ;; Save all buffers that are visiting files, without prompting.
  ;; (compile "/code/build/x64/Release/jai.exe /sokoban/src/first.jai -- x64")
  (compile "w:/handmade/build.bat")
)

(defun run-helper (key-sequence)
  (compile "w:/handmade/build/win32_handmade.exe")
)

(defun use-compile ()
  "Compile"
  (interactive)
  (compile-helper "<F7>"))

(defun use-run ()
  "Run"
  (interactive)
  (run-helper "<F7>"))

;(cd "w:/")

(defun my-toggle-source ()
  "Open the .cpp and .h of a file in side-by-side panes."
  (interactive)
  (command-execute 'delete-other-windows)
  (command-execute 'split-window-horizontally)
  (command-execute 'toggle-source)
  (command-execute 'other-window)
)

(defun my-next-window ()
  "Wrapper to call next-window"
  (interactive)
  (other-window 1 t)
)

(setq debug-on-error nil)

;; Assoc Direct3d FX file with c++ major mode.
(add-to-list 'auto-mode-alist '("\\.fx$" . c++mode))

;; ---------------
;; cutnpase of toggle_source.el to not copy the file everywhere.
;; https://github.com/emacsattic/toggle-source/blob/master/toggle-source.el
;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
(defvar toggle-source::mappings
  (list
   (cons ".h"  ".cpp")
   (cons ".h" ".cc")
   (cons ".hh" ".cxx")
   (cons ".rc" ".h")
   )
  "The file extensions that we toggle between.
If the current buffer contains a file that has an extension
in this list then we attempt to open the file with the
'other' extension.
(This only works in the current directory).")

(defun toggle-source ()
  "Toggle between source, and header files, and vice versa.
This is a handy function that allows you to toggle between pairs
of files that are both in the same directory.
The toggling is based upon the file extension, and by default
this is setup in to make it simple to toggle between header
and implementation files for C++ developers.

For example if you are currently editting a buffer containing
the contents of \"file.cpp\" running `toggle-source' would
take you to the buffer \"file.h\" - assuming this file existed."
  (interactive)
  (let ((filename (buffer-file-name))
    (list nil)
    (entry nil)
    (found nil))
    (setq list toggle-source::mappings)
    (while (and list (not found) filename)
      (setq entry (car list))
      (setq list  (cdr list))
      (if (string-match (concat (regexp-quote (car entry)) "$") filename)
      (setq found (cdr entry)))
      (if (string-match (concat (regexp-quote (cdr entry)) "$") filename)
      (setq found (car entry))))
    (if found
    (toggle-source::load (substring filename 0 (match-beginning 0)) found)
      )))

(defun toggle-source::load( file desiredExt )
  "Load the file that has the desired extension."
  (interactive)
  (setq file (concat file desiredExt))
  (if (file-exists-p file)
      (find-file file)))

(defun toggle-source-load-include-file ( FILE  )
  "Load the file FILE from the system include path.
The system include path is assumed to be stored
in the environmental variable INCLUDE.
 This function is handy for developers using C/C++,
and is included for completeness."
  (interactive "System include filename : ")
  (let ((includedirs
     ;; Different OS's use different ENV. seperators.
     (if (eq 'windows-nt system-type)
         (split-string (getenv "INCLUDE") ";")
       (split-string (getenv "INCLUDE") ":")))
    (dir nil)
    (finished nil))
    (while (and includedirs
        (not finished))
      (setq dir (car includedirs))
      (setq includedirs (cdr includedirs))
      (if (not (file-exists-p (concat dir "/" FILE )))
      ()
    (find-file (concat dir "/" FILE ))
    (setq finished t)))
    ))

;;; toggle-source.el ends here


(define-generic-mode
    'vars-mode
  '("#")
  '()
  '(("^[[:space:]]*:/.*" . 'font-lock-keyword-face)
    ("^[[:space:]]*[a-z][a-z0-9_]*" . 'font-lock-variable-name-face)
    ("[[:digit:]]*\\(\\.[[:digit:]]+\\)?" . font-lock-constant-face))
  '("\\.variables$")
  nil
  "A mode for .variables files")

;(global-set-key "\C-t" 'toggle-source)
(global-set-key "\M-t" 'toggle-source)

(global-set-key [?\C-u] 'upcase-word)

;; Change navigation keys.
(global-set-key [home] 'back-to-indentation)
(global-set-key [end] 'end-of-line)
;(global-set-key [next] (defun () (scroll-down 1)))
;(global-set-key [prior] (defun () (scroll-up 1)))
;(global-set-key [delete] 'delete-char)
(global-set-key [?\C-2] 'set-mark-command)
(global-set-key [f12] 'eval-buffer)
(global-set-key [f8] 'goto-line)
(global-set-key [f1] 'recompile)
(global-set-key [f5] 'use-run)
(global-set-key [f6] 'next-error)
(global-set-key [f7] 'use-compile)
(global-set-key [?\C-r] 'replace-string)
(global-set-key [?\M-r] 'query-replace)
(global-set-key [?\C-z] 'undo)
(global-set-key [?\C-\M-z] 'undo-redo)
(global-set-key [?\C-0] 'delete-window)
(global-set-key [?\C-1] 'delete-other-window)
(global-set-key [?\C-2] 'split-window-vertically)
(global-set-key [?\C-3] 'split-window-horizontally)
(global-set-key [?\C-o] 'my-next-window)
(global-set-key [C-backspace] 'kill-word)
(global-set-key [C-tab] 'next-window-any-frame)

(defun swap-lines-up ()
  "Exchange current line with the line above."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun swap-lines-down ()
  "Exchange current line with the line below."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-S-<up>") 'swap-lines-up)
(global-set-key (kbd "M-S-<down>") 'swap-lines-down)

(defun my/comment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
;; (global-set-key [?\C-/] 'my/comment) ;; where is undo then?
(global-set-key [?\C-\'] 'my/comment)

(defvar my/toggle-jump-dst-curr nil)
(defun my/toggle-jump-dst (dst at-dst)
  "Toggle position between dst and the current position."
  (interactive)
  (unless (boundp 'my/toggle-jump-dst-curr)
    (point-to-register 'my/toggle-jump-dst-curr))
  (cond ((funcall at-dst)
         (jump-to-register 'my/toggle-jump-dst-curr))
        (t (point-to-register 'my/toggle-jump-dst-curr)
           (funcall dst))))
(global-set-key [?\C-.]
                (lambda ()
                  (interactive)
                  (my/toggle-jump-dst
                   'end-of-buffer
                   (lambda () (= (line-number-at-pos)
                                 (line-number-at-pos (point-max)))))))
(global-set-key [?\C-,]
                (lambda ()
                  (interactive)
                  (my/toggle-jump-dst
                   'beginning-of-buffer
                   (lambda () (= (line-number-at-pos) (point-min))))))

;; ================================
;; Casey Muratori's С and C++ style.
; C++ indentation style
(defconst casey-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    ;(statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "Casey's Big Fun C++ Style")


; CC++ mode handling
(defun casey-big-fun-c-hook ()
  ; Set my style for the current buffer
  (c-add-style "BigFun" casey-big-fun-c-style t)

  ; 4-space tabs
  ;; (setq tab-width 4
  ;;       indent-tabs-mode nil)

  ; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ; Abbrevation expansion
  (abbrev-mode 1)

  (defun casey-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
        (if (file-exists-p (concat BaseFileName ".c"))
            (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))
  (defun casey-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'my/save-buffer)
  (define-key c++-mode-map (kbd "C-x C-s") 'my/save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  ;(define-key c++-mode-map [C-tab] 'indent-region)
  ;(define-key c++-mode-map " " 'indent-region)

  (define-key c++-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\e " 'set-mark-command)
  (define-key c++-mode-map "\eq" 'append-as-kill)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)
;; omit everything else
)
;; Casey's style end.
;; ================================
(add-hook 'c-mode-common-hook 'casey-big-fun-c-hook)
(add-hook 'c++-mode-common-hook 'casey-big-fun-c-hook)

(delete-selection-mode 1)

(defun my/save-buffer ()
  "Untabify, widen, remove trailing whitespace, then save buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (save-buffer)))
