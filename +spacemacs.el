;;; ~/.doom.d/+spacemacs.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ; Setup & load spacmeacs core packages ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Dot file location - SPC-f-e-d
(defun dotspacemacs/location ()
  ;; (doom/find-file-in-private-config)
  (doom/goto-private-config-file)
  )

;; (setq dotspacemacs-persistent-server t)
(load! "~/.doom.d/modules/spacemacs/core/core-funcs.el")
; (load! "~/.doom.d/modules/spacemacs/core/core-spacemacs-buffer.el")
;; (load! "~/.doom.d/modules/spacemacs/core/core-fonts-support.el")
;; (load! "~/.doom.d/modules/spacemacs/core/core-dumper.el")
(load! "~/.doom.d/modules/spacemacs/core/core-keybindings.el")
(load! "~/.doom.d/modules/spacemacs/core/core-transient-state.el")
(load! "~/.doom.d/modules/spacemacs/core/core-toggle.el")
;; (load! "~/.doom.d/modules/spacemacs/core/core-fonts-support.el")
;; TODO: evilified-state-evilify-map seems to have conflict with the doom
;; setting, like the leader key setting.
;; (use-package! evil-evilified-state
;;   :load-path
;;   "~/.doom.d/modules/spacemacs/evil-evilified-state")

;; (require 'bind-map)
;; (require 'core-funcs)
;; (require 'core-keybindings)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ; redifine the spacemcas/set-leader-keys macro ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro spacemacs/set-leader-keys (&rest rest)
  ;; (setq doom--map-fn 'doom--define-leader-key)
  ;; (doom--map-keyword-to-states :n)
  ;; (setq doom--map-state '(:n t))
  ;; (message rest)

  (doom--map-process (cons :leader (cons :n rest) ))
  ;; (doom--map-process (cons '(:n) 'rest))
  ;; (doom--map-process (apply 'concat :n  rest))
  )

;; (cond (t 6 7))
;; ( listp (cons :n 7) )

;; (spacemacs/set-leader-keys
;;   "s-;" 'vc-next-action)

;; (spacemacs/set-leader-keys
;; (map! :leader :n )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  unmap the doom original key prefix ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;  load the modified spacemacs layers packages   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "~/.doom.d/modules/spacemacs/layer/org/packages.el")
(load! "~/.doom.d/modules/spacemacs/layer/pdf/packages.el")
(load! "~/.doom.d/modules/spacemacs/layer/epub/packages.el")
(load! "~/.doom.d/modules/spacemacs/layer/dash/packages.el")
(load! "~/.doom.d/modules/spacemacs/layer/spacemacs-defaults/keybindings.el")
(load! "~/.doom.d/modules/spacemacs/layer/spacemacs-defaults/funcs.el")
;; (load! "~/.doom.d/modules/spacemacs/layer/version-control/packages.el")
;; (load! "~/.doom.d/modules/spacemacs/layer/version-control/keybindings.el")
;; (load! "~/.doom.d/modules/spacemacs/layer/version-control/funcs.el")
;; (load! "~/.doom.d/modules/spacemacs/layer/version-control/config.el")
(load! "~/.doom.d/modules/spacemacs/layer/github/packages.el")
(load! "~/.doom.d/modules/spacemacs/layer/github/funcs.el")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ; load the spacemacs original layers packages  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load! "~/.emacs.d.spacemacs/layers/+spacemacs/spacemacs-defaults/packages.el")
;; (load! "~/.emacs.d.spacemacs/layers/+spacemacs/spacemacs-defaults/keybindings.el")
;; (load! "~/.emacs.d.spacemacs/layers/+spacemacs/spacemacs-defaults/funcs.el")
;; (load! "~/.emacs.d.spacemacs/layers/+readers/pdf/packages.el")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         initialise layers          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
