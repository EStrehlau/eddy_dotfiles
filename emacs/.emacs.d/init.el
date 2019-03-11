;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 1000))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package oceanic-theme
  :ensure t
  :config
  (load-theme 'oceanic t))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq-default indent-tabs-mode nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("LANG" "LC_CTYPE"))
    ))
(use-package rbenv
  :ensure t
  :config
  (global-rbenv-mode))
(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))
(use-package  evil
  :ensure t
  :init
  (add-hook 'pdf-view-mode-hook (lambda () (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))
(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (setq evil-collection-mode-list (remove 'outline evil-collection-mode-list))
  (evil-collection-init)
  )
(use-package magit
  :ensure t
)
(use-package evil-magit
  :ensure t
  :after (evil magit)
  )
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package general
  :ensure t
  )
(use-package hydra
  :ensure t
  )
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  )
(use-package ace-window
  :ensure t
  )
(use-package org-noter
  :ensure t )
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  )
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )
(use-package ranger
  :ensure t)
(use-package ivy-hydra
  :ensure t)
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  )
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  )
                 
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
(enlarge-window arg)))
(defhydra hydra-window (:color red)
  "Window"
  ("h" windmove-left "move left")
  ("j" windmove-down "move down")
  ("k" windmove-up "move up")
  ("l" windmove-right "move right")
  ("H" hydra-move-splitter-left "grow left")
  ("J" hydra-move-splitter-down "grow down")
  ("K" hydra-move-splitter-up "grow left")
  ("L" hydra-move-splitter-right "grow right")
  ("+" text-scale-increase)
  ("ü" text-scale-decrease)
  ("d" delete-window)
  ("/" (lambda ()
                (interactive)
                (split-window-right)
                (windmove-right)) "split vertically")
  ("-" (lambda ()
                (interactive)
                (split-window-below)
                (windmove-down)) "split horizontally")
  ("q" nil "exit" :exit t)
  )
(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "p" '(:wk "Projectile" :keymap projectile-command-map)
 "h" '(:wk "Help" :keymap help-map)
 "o" 'ace-window
 "b" 'ivy-switch-buffer
 "k" 'org-capture
 "c" '(:ignore t)
 "c i" 'org-clock-in
 "c o" 'org-clock-out
 "c g" 'org-clock-goto
 "n" 'org-noter
 "a" 'org-agenda
 "B" 'ibuffer
 "f" 'counsel-find-file
 "/" 'swiper
 "w" 'hydra-window/body
 "r" 'hydra-projectile-rails/body
 "x" 'counsel-M-x
 )
(defun org-insertion-wrapper (f)
  (lambda (count)
    (interactive "P")
    (unless count (setq count 1))
    (dotimes (i count)
      (funcall f)
      )))
(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual insert)
"C-l" 'org-shiftmetaright
"C-k" 'org-move-subtree-up
"C-j" 'org-move-subtree-down
"C-h" 'org-shiftmetaleft
 "<M-return>" (org-insertion-wrapper 'org-meta-return)
 "<M-S-return>" (org-insertion-wrapper (lambda () (org-insert-todo-heading nil))))
(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual )
 ">" 'org-metaright
"L" 'org-forward-heading-same-level
"K" 'outline-previous-visible-heading
"J" 'outline-next-visible-heading
"H" 'org-backward-heading-same-level
"U" 'outline-up-heading
 "<" 'org-metaleft)
(setq org-capture-templates
      '(("t" "New Todo tasks.org" entry (file+headline "~/Sync/org/tasks.org" "Inbox")
         "* TODO %?")
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-filter-preset '("-someday"))
(setq org-agenda-include-diary t)
(setq org-agenda-files (quote
                        ("~/Sync/org/tasks.org"
                        "~/Sync/org/journal.org"
                        "~/Sync/org/notes"
                        )))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((org-agenda-files :maxlevel . 9) (nil :maxlevel . 9)))
(setq org-clock-into-drawer "CLOCK")
(setq org-clock-in-resume t)
(setq org-clock-out-when-done nil)
(setq org-clock-persist t)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-clock-report-include-clocking-task t)
(plist-put org-format-latex-options :scale 2.0)
(setq org-enforce-todo-dependencies t)
(setq c-basic-offset 4)
(setq-default tab-width 4)
(general-define-key
 :definer 'minor-mode
 :keymaps 'org-noter-doc-mode
 :states 'normal
 "i" 'org-noter-insert-note)
(add-to-list 'org-modules 'org-habit)
 
