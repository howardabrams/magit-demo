;;; magitdemo-demo --- Demonstration and Presentation for Magit

;;; Commentary:

;; Standard way of showing off an org-mode presentation followed with
;; a fully functional demonstration of Magit running with a repository
;; that "others" have been updating.

;;; Code:

(require 'demo-it)
(require 'org-tree-slide)

;; ----------------------------------------------------------------------
;;  Create each function that represents a "presentation frame"

(defun magitdemo-title-screen ()
  "Since presenting at PDX Emacs Meetup, we start with a screen to welcome other people to the show."
  (interactive)

  ;; On laptop, we need to do this on full screen:
  (demo-it-frame-fullscreen)
  ;; On computer, we want to have the screen fit for 1280 x 720
  ;;(set-frame-size nil 160 40)

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
  "Execute our source code in an Eshell buffer.  We'll demonstrate the complexity associated with the Git CLI, by running a few commands."
  (interactive)

  ;; The following commands will make the output of the git commands
  ;; prettier, and nicer to look at. However, they shouldn't be seen...
  (copy-file "/tmp/sandbox/my-proj/.git/config" "/tmp/sandbox/my-proj/.git/config.backup")
  (with-current-buffer (find-file-noselect "/tmp/sandbox/my-proj/.git/config")
    (goto-char (point-max))
    (insert "[color]
	status = always
	branch = always
	interactive = always
	ui = always
	diff = always")
    (newline)
    (save-buffer)
    (kill-buffer))

  (demo-it-start-eshell "/tmp/sandbox/my-proj" nil nil 'side)
  (toggle-truncate-lines 1)
  (sit-for 1.4)
  (demo-it-run-in-eshell "git status")
  (sit-for 2.5)
  (demo-it-run-in-eshell "git branch")
  (sit-for 3)
  (demo-it-run-in-eshell "git log --pretty=oneline")
  (sit-for 3)
  (demo-it-run-in-eshell "git show HEAD~3"))

(defun magitdemo-run-git-error ()
  "Display a git error message to see how it helps."
  (interactive)
  (demo-it-run-in-eshell "git checkout master -force"))

(defun magitdemo-run-git-long ()
  "Display a long, long command with an alias."
  (interactive)
  ;; We want the typing of the command to wrap, but as soon as we are
  ;; done, we want it back to looking nicer on the screen.
  (toggle-truncate-lines nil)
  (demo-it-run-in-eshell "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative")
  (insert "
")
  (toggle-truncate-lines 1)
  (eshell-send-input))

(defun magitdemo-start-magit ()
  "Start magit on the conflicted clone of a Git repository."
  (interactive)
  (if (file-exists-p "/tmp/sandbox/my-proj/.git/config.backup")
      (rename-file "/tmp/sandbox/my-proj/.git/config.backup" "/tmp/sandbox/my-proj/.git/config" t))
  (magit-status "/tmp/sandbox/my-proj")
  (text-scale-set 3)

  ;; After a couple of seconds to adjust, open up a window to display
  ;; the keys that I type:
  (sit-for 5)
  (mwe:log-keyboard-commands 1)
  (split-window-horizontally)
  (mwe:open-command-log-buffer)
  (text-scale-set 2)
  (delete-region (point-min) (point-max))
  ;; (highlight-regexp "^[A-z0-9<>$?-]+\\( [A-z0-9<>$?-]+\\)?" 'hi-green-b)
  (enlarge-window-horizontally -15)

  (other-window 1))

(defun magitdemo-next-magit ()
  "Start magit on a nicer clone of a Git repository."
  (interactive)
  (magit-status "/tmp/sandbox/da-quotes")
  (text-scale-set 3)

  (sit-for 5)
  (mwe:log-keyboard-commands 1)
  (split-window-horizontally)
  (mwe:open-command-log-buffer)
  (enlarge-window-horizontally -15)

  (other-window 1))

;; At what points do I want to type, but not fat-finger it?
(setq demo-it-text-entries
      #s(hash-table data
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

  ;; Let's make sure that the eshell that I start to demonstration git
  ;; commands has been deleted and cleaned up.
  (if (member "Shell" (mapcar (lambda (b) (buffer-name b)) (buffer-list)))
      (kill-buffer "Shell"))

  ;; Let's create the two git repositories by running our shell scripts:
  (shell-command "bash ./gitrepo-history-conflict.sh")
  (shell-command "bash ./gitrepo-history-nice.sh")
  (kill-buffer "*Shell Command Output*")

  ;; We want to make sure that all of the Magit buffers and windows
  ;; are logging their key sequences to the mwe:log-keyboard-commands
  ;; feature.
  (mapcar (lambda (h) (add-hook h 'mwe:log-keyboard-commands))
          (list 'magit-mode-hook
                'magit-diff-mode-hook
                'magit-popup-mode-hook
                'magit-ediff-quit-mode-hook
                'magit-log-mode-hook
                'magit-process-mode-hook))

  (menu-bar-mode 0)

  (demo-it-start (list
                  'magitdemo-title-screen      ; Frame 1
                  'magitdemo-load-presentation ; Frame 2
                  'magitdemo-run-git-shell     ; Frame 3
                  'magitdemo-run-git-long      ; Frame 4
                  'magitdemo-run-git-error     ; Frame 5
                  'demo-it-presentation-return ; Frame 6
                  'magitdemo-start-magit       ; Frame 7
                  'demo-it-presentation-return ; Frame 8
                  'magitdemo-next-magit        ; Frame 9
                  'demo-it-presentation-return) t))

;; ----------------------------------------------------------------------
;; Start the presentation whenever this script is evaluated. Good idea?
(magitdemo-start-presentation)

;;; magit-demo.el ends here
