;;; +spacemacs.el -*- lexical-binding: t; -*-

;;; Setup & load spacmeacs core packages

(if (not (bound-and-true-p spacemacs-path))
    (setq spacemacs-path "~/.doom.d/modules/"))

(setq dotspacemacs-editing-style 'vim)
(setq dotspacemacs-emacs-command-key "SPC")
(setq dotspacemacs-emacs-leader-key "M-m")
(setq dotspacemacs-ex-command-key ":")
(setq dotspacemacs-leader-key "SPC")
(setq dotspacemacs-major-mode-leader-key ",")
(setq dotspacemacs-major-mode-emacs-leader-key "C-M-m")
(setq spacemacs-cache-directory doom-cache-dir)
(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.1)

(defun dotspacemacs/location ()
  "Dot file location - SPC-f-e-d"
  ;; (doom/find-file-in-private-config)
  (doom/goto-private-config-file))

;; (setq dotspacemacs-persistent-server t)
(load! (concat spacemacs-path "spacemacs/core/core-funcs.el"))
; (load! (concat spacemacs-path "spacemacs/core/core-spacemacs-buffer.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-fonts-support.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-dumper.el")
(load! (concat spacemacs-path "spacemacs/core/core-keybindings.el"))
(load! (concat spacemacs-path "spacemacs/core/core-transient-state.el"))
(load! (concat spacemacs-path "spacemacs/core/core-toggle.el"))
;; (load! (concat spacemacs-path "spacemacs/core/core-hooks.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-fonts-support.el")
;; TODO: evilified-state-evilify-map seems to have conflict with the doom
;; setting, like the leader key setting.
;; (use-package! evil-evilified-state
;;   :load-path
;;   (concat spacemacs-path "evil-evilified-state")

;; (require 'bind-map)
;; (require 'core-funcs)
;; (require 'core-keybindings)

(defvar dotspacemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of Emacs.")

(defvar spacemacs-post-user-config-hook nil
  "Hook run after dotspacemacs/user-config")
(defvar spacemacs-post-user-config-hook-run nil
  "Whether `spacemacs-post-user-config-hook' has been run")
(defvar dotspacemacs-show-transient-state-title t
  "If non nil show the titles of transient states.")
(defvar dotspacemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")
(setq spacemacs-post-user-config-hook-run t)
(defun spacemacs/defer-until-after-user-config (func)
  "Call FUNC if dotspacemacs/user-config has been called. Otherwise,
defer call using `spacemacs-post-user-config-hook'."
  (if spacemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'spacemacs-post-user-config-hook func)))

(setq version-control-diff-tool 'git-gutter)

(defmacro spacemacs/set-leader-keys (&rest rest)
  "redefine the spacemcas/set-leader-keys macro"
  ;; (setq doom--map-fn 'doom--define-leader-key)
  ;; (doom--map-keyword-to-states :n)
  ;; (setq doom--map-state '(:n t))
  ;; (message rest)
  (doom--map-process (cons :leader (cons :n rest) ))
  ;; (doom--map-process (cons '(:n) 'rest))
  ;; (doom--map-process (apply 'concat :n  rest))
  )

(defvar dotspacemacs-default-layout-name "Default"
  "Name of the default layout.")
(defvar dotspacemacs-auto-generate-layout-names nil
  "If non-nil, auto-generate layout name when creating new layouts.
Only has effect when using the \"jump to layout by number\" commands.")
(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (+doom-dashboard/open (selected-frame))
  (delete-other-windows))


;;; unmap the doom original key prefix
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


;;;  load the modified spacemacs layers packages
(load! (concat spacemacs-path "spacemacs/layer/org/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/pdf/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/epub/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/dash/packages.el"))
;; (load! (concat spacemacs-path "spacemacs/layer/dash/config.el"))
;; (load! (concat spacemacs-path "spacemacs/layer/dash/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-defaults/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-defaults/keybindings.el"))
;; (load! (concat spacemacs-path "spacemacs/layer/version-control/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/version-control/keybindings.el"))
(load! (concat spacemacs-path "spacemacs/layer/version-control/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/version-control/config.el"))
(load! (concat spacemacs-path "spacemacs/layer/github/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/github/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-navigation/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-navigation/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-layouts/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-layouts/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-layouts/config.el"))
(load! (concat spacemacs-path "spacemacs/layer/bm/packages.el"))

(load! (concat spacemacs-path "spacemacs/layer/git/packages.el"))

(load! (concat spacemacs-path "spacemacs/layer/javascript/packages.el"))
(load! (concat spacemacs-path "spacemacs/layer/javascript/config.el"))
(load! (concat spacemacs-path "spacemacs/layer/javascript/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/spacemacs-editing/packages.el"))

;;; initialise layers

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

(org/init-org)
;; (org/post-init-org)
(org/init-org-agenda)
(org/init-org-brain)
(org/init-org-expiry)
(org/init-org-download)
(org/init-org-jira)
(org/init-org-mime)
(org/init-org-pomodoro)
(org/init-org-present)
(org/init-org-cliplink)
;; (org/init-org-projectile)
;; (org/pre-init-org-re-reveal)
;; (org/init-org-re-reveal )
(org/init-org-journal)
;; (org/init-org-trello)
;; (org/init-org-sticky-header)

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

(github/init-forge)
(github/init-gist)
(github/init-github-clone)
(github/init-github-search)
;github/init-spacemacs-github ()


(pdf/init-pdf-tools)
(epub/init-nov)
(dash/init-dash-at-point)
;;(dash/init-counsel-dash)


(setq hydra--work-around-dedicated nil) ; help lv to work https://github.com/abo-abo/hydra/issues/329, even setup this, lv can still has bug with SPC-l-n or SPC-l-p
;; "message" is flickering when SPC-w.-[or]
;;(setq hydra-hint-display-type 'message)
;; since "lv" is not work well on doom even set work around to nil, SPC-l-n or p will create relundant windows
(setq hydra-hint-display-type 'lv)
;; posframe seems great
;;(setq hydra-hint-display-type 'posframe)
;; (setq hydra--work-around-dedicated nil)
(spacemacs-navigation/init-auto-highlight-symbol)
(spacemacs-navigation/init-symbol-overlay)
(spacemacs-layouts/init-eyebrowse)
(spacemacs-layouts/init-persp-mode)
(bm/init-bm)
(git/init-git-timemachine)

(javascript/init-js2-mode)
;; you can only choose either nodejs or skewer
(setq javascript-repl 'nodejs)          ; choose nodejs
(javascript/init-nodejs-repl)
;;(setq javascript-repl 'skewer)        ; choose skewer
;;(javascript/init-skewer-mode)

(spacemacs-editing/init-avy)
(spacemacs-editing/init-expand-region)
(spacemacs-editing/init-link-hint)
(spacemacs-editing/init-move-text)
(spacemacs-editing/init-string-inflection)

(setq dotspacemacs-folding-method 'evil)
