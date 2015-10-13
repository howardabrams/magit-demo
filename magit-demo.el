;;; magitdemo-demo --- Demonstration and Presentation for Magit

;;; Commentary:

;; Standard way of showing off an org-mode presentation with a few
;; command features. We may want to have a fully functional git
;; repository that we can play around with.

;;; Code:

(require 'demo-it)
(require 'org-tree-slide)

;; ----------------------------------------------------------------------
;;  Create each function that represents a "presentation frame"

(defun magitdemo-title-screen ()
  (interactive)
  (demo-it-frame-fullscreen)
  ;;(set-frame-size nil 160 40)   ; Fit for 1280 x 720
  (demo-it-title-screen "sub-title.org" 5)
  (split-window-below)
  (demo-it-show-image "pdx-emacs.png" nil)
  (other-window 1)
  (enlarge-window 1)
  (message "Let's hear it for our first meeting!"))

(defun magitdemo-load-presentation ()
  "Display magitdemo-demo.org (an 'org-mode' file) as a presentation."
  (delete-other-windows)
  (org-tree-slide-simple-profile)
  (demo-it-presentation "presentation.org" 5))

(defun magitdemo-run-git-shell ()
  "Execute our source code in an Eshell buffer."
  (interactive)
  (demo-it-start-eshell "/tmp/git-sandbox/my-proj" nil nil 'side)
  (toggle-truncate-lines 1)
  (sit-for 1.4)
  (demo-it-run-in-eshell "git status")
  (sit-for 2.5)
  (demo-it-run-in-eshell "git branch")
  (sit-for 3)
  (demo-it-run-in-eshell "git log --pretty=oneline")
  (sit-for 3)
  (demo-it-run-in-eshell "git show --color HEAD~3"))

(defun magitdemo-run-git-error ()
  "Display a git error message to see how it helps."
  (interactive)
  (demo-it-run-in-eshell "git checkout master -force"))

(defun magitdemo-run-git-long ()
  "Display a long, long command with an alias."
  (interactive)
  (toggle-truncate-lines nil)
  (demo-it-run-in-eshell "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative")
  (insert "
")
  (toggle-truncate-lines 1)
  (eshell-send-input))

(defun magitdemo-start-magit ()
  (interactive)
  (magit-status "/tmp/git-sandbox/my-proj")
  (mwe:log-keyboard-commands 1)
  (text-scale-increase 2)
  (sit-for 5)
  (split-window-horizontally)
  (mwe:open-command-log-buffer)
  (text-scale-increase 1)
  ;; (highlight-regexp "^[A-z0-9<>$?-]+\\( [A-z0-9<>$?-]+\\)?" 'hi-green-b)
  (enlarge-window-horizontally -15)
  (other-window 1))

(defun magitdemo-next-magit ()
  (interactive)
  (magit-status "/tmp/git-sandbox/da-quotes")
  (mwe:log-keyboard-commands 1)
  (text-scale-increase 2)
  (sit-for 5)
  (split-window-horizontally)
  (mwe:open-command-log-buffer)
  (enlarge-window-horizontally -15)
  (other-window 1))

(setq demo-it-text-entries #s(hash-table data
                                         (?1 "Adding both a license (finally) as well as a few more notes to the README."
                                             ?2 "Add a few notes to the README.

This commit also includes a license file (finally).
Should we actually complete this project?"
                                             ?3 "Best quote from Life of Brian")))

;; ----------------------------------------------------------------------
;; Demonstration and/or Presentation Order

(defun magitdemo-start-presentation ()
  "Demonstration and Presentation for Magit."
  (interactive)
  (demo-it-keybindings)

  (if (member "Shell" (mapcar (lambda (b) (buffer-name b)) (buffer-list)))
      (kill-buffer "Shell"))

  (shell-command "bash ./gitrepo-history-conflict.sh")
  (shell-command "bash ./gitrepo-history-nice.sh")
  (kill-buffer "*Shell Command Output*")

  (mapcar (lambda (h) (add-hook h 'mwe:log-keyboard-commands))
          (list 'magit-mode-hook
                'magit-diff-mode-hook
                'magit-popup-mode-hook
                'magit-ediff-quit-mode-hook
                'magit-log-mode-hook
                'magit-process-mode-hook))

  (global-unset-key (kbd "<f1>"))
  (menu-bar-mode 0)
  ;; (global-set-key (kbd "<f1>") 'demo-it-step)

  (demo-it-start (list
                  'magitdemo-title-screen
                  'magitdemo-load-presentation ; Frame 1
                  'magitdemo-run-git-shell     ; Frame 2
                  'magitdemo-run-git-long      ; Frame 3
                  'magitdemo-run-git-error     ; Frame 4
                  'magitdemo-start-magit
                  'demo-it-presentation-return
                  'magitdemo-next-magit
                  'demo-it-presentation-return) t))

;; ----------------------------------------------------------------------
;; Start the presentation whenever this script is evaluated. Good idea?
(magitdemo-start-presentation)

;;; magit-demo.el ends here
