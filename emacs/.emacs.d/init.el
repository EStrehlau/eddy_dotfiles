(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq-default indent-tabs-mode nil)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq gnus-select-method '(nntp "news.newshosting.com"))
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package  evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (add-hook 'pdf-view-mode-hook (lambda () (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))
  :config
  (evil-mode 1))
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))
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
(use-package oceanic-theme
  :ensure t
  :config
  (load-theme 'oceanic t))
(use-package
projectile
  :ensure t
  :config
  (projectile-mode +1)
  )
(use-package syndicate
  :ensure t)
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))
(use-package ranger
  :ensure t)
                 
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
 "o" 'ace-window
 "b" 'ivy-switch-buffer
 "B" 'ibuffer
 "f" 'counsel-find-file
 "/" 'swiper
 "w" 'hydra-window/body
 )
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-collection oceanic-theme use-package pdf-tools org-noter exec-path-from-shell evil ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
