;;; private/spacemacs/simple.el -*- lexical-binding: t; -*-

;;; Define spacemacs-module-path
;; use `load-file-name' to determine the parent directory
(defvar spacemacs-module-path (file-name-directory load-file-name))

;;; Redefine Missing spacemacs variables & functions

(defvar spacemacs-version doom-version)
(defvar spacemacs-post-user-config-hook nil
  "Hook run after dotspacemacs/user-config")
(defvar spacemacs-post-user-config-hook-run nil
  "Whether `spacemacs-post-user-config-hook' has been run")
(defun spacemacs/defer-until-after-user-config (func)
  "Call FUNC if dotspacemacs/user-config has been called. Otherwise,
defer call using `spacemacs-post-user-config-hook'."
  (if spacemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'spacemacs-post-user-config-hook func)))

;;; Load & setup core-load-paths

(load! (concat spacemacs-module-path "core/core-load-paths.el"))
(setq spacemacs-cache-directory doom-cache-dir) ; setup load path

;;; Load & setup core-dotspacemacs

(load! (concat spacemacs-module-path "core/core-dotspacemacs.el"))
(defun dotspacemacs/init ()
  "Supported dotspacemacs settings."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-leader-key "SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distribution 'doom
   dotspacemacs-startup-banner 100
   dotspacemacs-folding-method 'evil
   dotspacemacs-filepath (expand-file-name "config.el" doom-private-dir)
   dotspacemacs-startup-lists '((recents  . 12)
                                ;; (bookmarks . 8)
                                ;; (projects . 7)
                                ;; (agenda . 7)
                                ;; (todos . 7)
                                )))
(dotspacemacs/init)
(setq spacemacs-initialized t)

;; Overwrite doom related key prefixes
(setq doom-leader-key dotspacemacs-leader-key)
(setq doom-leader-alt-key dotspacemacs-emacs-leader-key)
(setq doom-localleader-key dotspacemacs-major-mode-leader-key)
(setq doom-localleader-alt-key dotspacemacs-major-mode-emacs-leader-key)

;;; Load & Setup core-spacemacs-buffer.el

(load! (concat spacemacs-module-path "core/libs/page-break-lines.el"))
(require 'page-break-lines)
(load! (concat spacemacs-module-path "core/core-spacemacs-buffer.el"))
(setq spacemacs-buffer-name "*Home*") ; setup the dashboard name

;;; Load & Setup other core libraries

(load! (concat spacemacs-module-path "core/core-funcs.el"))
(load! (concat spacemacs-module-path "core/core-keybindings.el"))
(load! (concat spacemacs-module-path "core/core-transient-state.el"))
(load! (concat spacemacs-module-path "core/core-toggle.el"))
(setq spacemacs-post-user-config-hook-run t)

;;; Other Configurations

(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.1)
(setq version-control-diff-tool 'git-gutter)

;;; map

;; unmap the doom original key prefix
(map! :leader
      "gg" nil
      "bN" nil
      "bN" nil
      "fe" nil
      "fC" nil
      "fy" nil
      "hP" nil
      "hT" nil
      "tt" nil
      "wc" nil
      "x" nil)

;; unbind the keys when necessary
(general-auto-unbind-keys)

(defmacro spacemacs/set-leader-keys (&rest rest)
  "Redefine the spacemcas/set-leader-keys macro"
  (doom--map-process (cons :leader (cons :n rest) )))

(defmacro spacemacs/set-leader-keys-for-major-mode (mode &rest rest)
  "Redefine the `spacemacs/set-leader-keys-for-major-mode' macro.
Spacemacs use spacemacs-.*-mode-map, we here use original mode map."
  `(define-localleader-key! :keymaps '(,(intern (replace-regexp-in-string "'" "" (format "%s-map" mode)))) ,@rest))

(defmacro spacemacs/set-leader-keys-for-minor-mode (mode &rest rest)
  "Redefine the `spacemacs/set-leader-keys-for-major-mode' macro.
Spacemacs use spacemacs-.*-mode-map, we here use original mode map."
  `(define-localleader-key! :keymaps '(,(intern (replace-regexp-in-string "'" "" (format "%s-map" mode)))) ,@rest))

;; define some simple but important keys with map! which is easier than using
;; spacemacs ways to define.
(map! :leader
      (:when (featurep 'ivy)
       :desc "M-x"                     :n "SPC" #'counsel-M-x))
(map! :leader
      (:when (featurep 'helm)
       :desc "M-x"                     :n "SPC" #'helm-M-x))

;;; Layers

;; load the modified spacemacs layers packages
;; initialise layers

;; org layer

(load! (concat spacemacs-module-path "layer/org/config.el"))
(load! (concat spacemacs-module-path "layer/org/funcs.el"))
(load! (concat spacemacs-module-path "layer/org/packages.el"))
(org/init-org)
;; (org/post-init-org)
;; (org/init-org-agenda)
;; (org/init-org-brain)
;; (org/init-org-expiry)
;; (org/init-org-download)
;; (org/init-org-jira)
;; (org/init-org-mime)
;; (org/init-org-pomodoro)
;; (org/init-org-present)
;; (org/init-org-cliplink)
;; (org/init-org-projectile)
;; (org/pre-init-org-re-reveal)
;; (org/init-org-re-reveal )
;; (org/init-org-journal)
;; (org/init-org-trello)
;; (org/init-org-sticky-header)
(setq org-want-todo-bindings t)
(org/init-evil-org)


;; pdf layer
(load! (concat spacemacs-module-path "layer/pdf/packages.el"))
(pdf/init-pdf-tools)

;; epub layer
(load! (concat spacemacs-module-path "layer/epub/packages.el"))
(epub/init-nov)

;; dash layer

(load! (concat spacemacs-module-path "layer/dash/packages.el"))
;; (load! (concat spacemacs-module-path "layer/dash/config.el"))
;; (load! (concat spacemacs-module-path "layer/dash/funcs.el"))
(dash/init-dash-at-point)
;;(dash/init-counsel-dash)



;; spacemacs defaults layer
(load! (concat spacemacs-module-path "layer/spacemacs-defaults/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-defaults/keybindings.el"))
                                        ; (spacemacs-defaults/init-abbrev)
                                        ; (spacemacs-defaults/init-archive-mode)
                                        ; (spacemacs-defaults/init-bookmark)
                                        ; (spacemacs-defaults/init-conf-mode)
                                        ; (spacemacs-defaults/init-dired)
                                        ; (spacemacs-defaults/init-dired-x)
                                        ; (spacemacs-defaults/init-electric-indent-mode)
                                        ; (spacemacs-defaults/init-visual-line-mode)
                                        ; (spacemacs-defaults/init-ediff)
;; (spacemacs-defaults/init-eldoc)
                                        ; (spacemacs-defaults/init-help-fns+)
                                        ; (spacemacs-defaults/init-hi-lock)
                                        ; (spacemacs-defaults/init-image-mode)
                                        ; (spacemacs-defaults/init-imenu)
;; (spacemacs-defaults/init-display-line-numbers)
;; (spacemacs-defaults/init-linum)
                                        ; (spacemacs-defaults/init-occur-mode)
                                        ; (spacemacs-defaults/init-package-menu)
;; (spacemacs-defaults/init-page-break-lines)
                                        ; (spacemacs-defaults/init-process-menu)
;; (spacemacs-defaults/init-recentf)
;; (spacemacs-defaults/init-savehist)
;; (spacemacs-defaults/init-saveplace)
;; (spacemacs-defaults/init-subword)
;; (spacemacs-defaults/init-tar-mode)
;; (spacemacs-defaults/init-uniquify)
;; (spacemacs-defaults/init-url)
                                        ; (spacemacs-defaults/init-whitespace)
                                        ; (spacemacs-defaults/init-winner)
;; (spacemacs-defaults/init-zone)
(setq hydra--work-around-dedicated nil) ; help lv to work https://github.com/abo-abo/hydra/issues/329, even setup this, lv can still has bug with SPC-l-n or SPC-l-p
;; "message" is flickering when SPC-w.-[or]
;;(setq hydra-hint-display-type 'message)
;; since "lv" is not work well on doom even set work around to nil, SPC-l-n or p will create relundant windows
(setq hydra-hint-display-type 'lv)
;; posframe seems great
;;(setq hydra-hint-display-type 'posframe)
;; (setq hydra--work-around-dedicated nil)



;; version control layer
;; (load! (concat spacemacs-module-path "layer/version-control/packages.el"))
(load! (concat spacemacs-module-path "layer/version-control/keybindings.el"))
(load! (concat spacemacs-module-path "layer/version-control/funcs.el"))
(load! (concat spacemacs-module-path "layer/version-control/config.el"))
;; (version-control/init-vc)
;; (version-control/init-diff-mode)
;; (version-control/init-diff-hl)
;; (version-control/post-init-evil-unimpaired)
;; (version-control/init-git-gutter)
;; ;;(version-control/init-git-gutter-fringe)
;; (version-control/init-git-gutter+)
;; ;;(version-control/init-git-gutter-fringe+)
;; (version-control/init-smerge-mode)
;; (version-control/init-browse-at-remote)


;; github layer
(load! (concat spacemacs-module-path "layer/github/funcs.el"))
(load! (concat spacemacs-module-path "layer/github/packages.el"))

(github/init-forge)
(github/init-gist)
(github/init-github-clone)
(github/init-github-search)
;; github/init-spacemacs-github ()


;; spacemacs navigation layer
(load! (concat spacemacs-module-path "layer/spacemacs-navigation/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-navigation/packages.el"))
(spacemacs-navigation/init-auto-highlight-symbol)
(spacemacs-navigation/init-symbol-overlay)


;; spacemacs layouts layer
(load! (concat spacemacs-module-path "layer/spacemacs-layouts/packages.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-layouts/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-layouts/config.el"))
(spacemacs-layouts/init-eyebrowse)
(spacemacs-layouts/init-persp-mode)


;; bm layer
(load! (concat spacemacs-module-path "layer/bm/packages.el"))
(bm/init-bm)


;; git layer

(load! (concat spacemacs-module-path "layer/git/packages.el"))
(git/init-git-timemachine)

;; javascript layer

(load! (concat spacemacs-module-path "layer/javascript/packages.el"))
(load! (concat spacemacs-module-path "layer/javascript/config.el"))
(load! (concat spacemacs-module-path "layer/javascript/funcs.el"))
(javascript/init-js2-mode)
;; you can only choose either nodejs or skewer
(setq javascript-repl 'nodejs)          ; choose nodejs
(javascript/init-nodejs-repl)
;;(setq javascript-repl 'skewer)        ; choose skewer
;;(javascript/init-skewer-mode)


;; spacemacs editing layer
(load! (concat spacemacs-module-path "layer/spacemacs-editing/packages.el"))
(spacemacs-editing/init-avy)
(spacemacs-editing/init-expand-region)
(spacemacs-editing/init-link-hint)

(spacemacs-editing/init-move-text)
(spacemacs-editing/init-string-inflection)

;; spacemacs evil layer

(load! (concat spacemacs-module-path "layer/spacemacs-evil/config.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-evil/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-evil/packages.el"))
;; (spacemacs-evil/init-evil-anzu)
;; (spacemacs-evil/init-evil-args)
;; (spacemacs-evil/init-evil-cleverparens)
;; (spacemacs-evil/init-evil-ediff)
;; (spacemacs-evil/init-evil-escape)
;; (spacemacs-evil/init-evil-exchange)
;; (spacemacs-evil/init-evil-goggles)
(spacemacs-evil/init-evil-iedit-state)
;; (spacemacs-evil/init-evil-indent-plus)
(spacemacs-evil/init-evil-lion)
;;(spacemacs-evil/init-evil-lisp-state)
(spacemacs-evil/init-evil-nerd-commenter)
;; (spacemacs-evil/init-evil-matchit)
(spacemacs-evil/init-evil-numbers)
;; (spacemacs-evil/init-evil-surround)
(spacemacs-evil/init-evil-terminal-cursor-changer)
;; (spacemacs-evil/init-evil-textobj-line)
;; (spacemacs-evil/init-evil-tutor)
;; (spacemacs-evil/init-evil-unimpaired)
;; (spacemacs-evil/init-evil-visual-mark-mode)
;; (spacemacs-evil/init-evil-visualstar)
;; (spacemacs-evil/init-hs-minor-mode)
;; (spacemacs-evil/init-linum-relative)
;; (spacemacs-evil/init-vi-tilde-fringe)


;; shell layer
(load! (concat spacemacs-module-path "layer/shell/config.el"))
(load! (concat spacemacs-module-path "layer/shell/funcs.el"))
(load! (concat spacemacs-module-path "layer/shell/packages.el"))
(shell/init-eshell)
(shell/init-vterm)

;; json layer
(load! (concat spacemacs-module-path "layer/json/config.el"))
(load! (concat spacemacs-module-path "layer/json/funcs.el"))
(load! (concat spacemacs-module-path "layer/json/packages.el"))
(json/init-json-navigator)
(json/init-json-reformat)
(json/init-json-snatcher)

(defalias 'iedit-lib-cleanup 'iedit-cleanup)

;; ivy layer
;; if default +bindings is enabled in init.el, it will not run


(load! (concat spacemacs-module-path "layer/ivy/packages.el"))
(load! (concat spacemacs-module-path "layer/ivy/config.el"))
(load! (concat spacemacs-module-path "layer/ivy/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-project/packages.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-project/funcs.el"))
(load! (concat spacemacs-module-path "layer/spacemacs-completion/funcs.el"))
(map! :leader
      "sg" nil
      "sk" nil
      "sr" nil
      "st" nil)
(ivy/post-init-bookmark)
(ivy/init-counsel)
(ivy/pre-init-counsel-projectile)
(ivy/post-init-evil)
(if (featurep 'helm-make)
    (ivy/init-helm-make))
(ivy/post-init-imenu)
(ivy/init-ivy)
(ivy/post-init-projectile)
(ivy/init-swiper)
(ivy/init-wgrep)
(spacemacs-project/init-projectile)
(spacemacs//ivy-hjkl-navigation dotspacemacs-editing-style)


;; it is necessary to require cl even >=27.1
;; otherwise some functions name will be missing
(require 'cl)
