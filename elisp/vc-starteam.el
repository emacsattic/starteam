;;; vc-starteam.el --- VC-type mode for StarTeam

;;; Copyright (C) 2002 Matthew O. Smith
;;; Copyright (C) 2001 Nearlife, Inc.
;;; Copyright (C) 1999 Matthew O. Smith

;; Filename: vc-starteam.el
;; Maintainer: Matthew O. Smith <m0smith at yahoo dot com> 
;; Author: $Author$
;; Last-Updated: $Date$ 
;; Version: $Revision$
;; Keywords: tools
;; URL: http://sourceforge.net/projects/starteam-el/
;; 
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This files implements a StarTeam (www.starbase.com) backend for the VC
;; module.  See vc.el for more information.
;;
;; Configuration:
;;
;; At a minimum you need to set the following:
;;
;;   (setq vc-starteam-host "10.42.42.2"   ; or "myMachine.myCompany.com"
;;         vc-starteam-executable "stcmd"
;;         vc-starteam-to-directory-alist (list (cons  "^/export/home/username/working"  "src/src" )))
;;
;;
;;    Uses dired-omit-files to tell VC what files are not under source control
;;    and are not to be under source control.
;;    (require 'dired-x)
;;	  (setq dired-omit-files
;;		  (concat dired-omit-files "\\|^TAGS$\\|.war$\\|.class$\\|.cache$\\|.flc$\\|^.dired$"))
;;
;; You will need to customize vc-starteam-to-directory-alist to your
;; particular Starteam projects; see the doc comments for more
;; details on the structure of this variable.
;;
;; In addition, there are also several other variables that configure
;; the behavior of vc-starteam.el:
;;
;;    vc-starteam-dired-mode-name
;;    vc-starteam-port
;;    vc-starteam-user
;;    vc-starteam-locking-user
;;    vc-starteam-password
;;    vc-starteam-keep-unlocked-files-read-only
;;    vc-starteam-unlock-after-checkin
;;    vc-starteam-switch-to-output
;;    vc-starteam-debug
;;    vc-starteam-post-checkin-functions
;;
;; Users may also wish to call 
;;  
;;    vc-starteam-menubar-setup 
;;
;; to add a STARTEAM menu to the main menu bar. 

;;; History:
;;
;; 0.1    [Matthew O. Smith <m0smith at yahoo dot com>] 
;;        A good first cut
;;
;; 0.2    [Matthew O. Smith] 
;;        Works on a Solaris box
;;
;; 0.3    [Christopher J. Kline <chris at nearlife dot com>] 08 May 2001 
;;        Many changes:
;;           - Matthew Smith's company no longer uses Starteam; Chris Kline taking 
;;             over maintenance
;;           - Improved documentation on variables and methods
;;           - Added configurable option to keep unlocked files read-only (check-out 
;;             also honors this option)
;;           - Added configurable option to unlock files after check-in
;;           - Added configurable option for debugging (echoes each command-line 
;;             operation to *Messages*)
;;           - Behavior of buffer-switching to output of query operations (e.g., 
;;             getting file history) is now configurable via vc-starteam-switch-to-output
;;           - Added vc-starteam-unlock-file
;;           - Added vc-starteam-menubar-setup to add STARTEAM menu to main menubar
;;           - Check for failure of most operations, output descriptive error messages
;;           - If check-out fails, reason is given and option to force check-out is 
;;             presented
;;           - Most operations now fail gracefully on common failure conditions 
;;             (usually when buffer file cannot be reconciled with 
;;             vc-starteam-to-directory-alist)
;;           - Functions now restore current-buffer to original state before exiting
;;             (except vc-starteam-actual-buffer-current-activity; see "Known Bugs")
;;           - Consolidated redundant code
;;
;; 0.4    [Christopher J. Kline] 09 May 2001         
;;        - vc-starteam-perform-lock-operation now checks to make sure
;;          that the operation succeeded; if it does not, will try to
;;          provide an explanation of why the operation failed
;;        - changed (file (buffer-name buf)) to 
;;          (file (file-name-nondirectory (buffer-file-name buf))) in
;;          vc-starteam-current-buffer-dir-and-file so that file name
;;          returned is based on the file associated with the buffer
;;          instead of the buffer name (fixed previous bug which
;;          prevented operations from working when the 'uniquify'
;;          package was operating)
;;
;; 0.5    [Christopher J. Kline] 21 June 2001         
;;        - fixed bug in vc-starteam-dired-mode where "Unknown" status
;;          or a "-" in the file mode would cause a "no file this line" 
;;          error 
;;
;; 0.6    [Matthew O. Smith] 08 May 2002
;;        - added vc-starteam-checkout-dir-recursive
;;        - Chris Kline no longer has access to Starteam, and Matthew Smith has
;;          resumed using it, so Matthew is taking over maintenance of vc-starteam.el
;;
;; 0.7    [Matthew O. Smith] 
;;        - Now on sourceforge: http://sourceforge.net/projects/starteam-el/
;;        - 560193: Corrected directory handling in vc-starteam-checkout-dir-recursive
;;        - Formatted the output to be in dired mode
;;        - Rename from starteam.el to vc-starteam.el for integration into
;;          vc-mode
;;        - 554531 - Have get-directory go into a dired mode.
;;        - register works
;;        - Merge files as needed
;;        - Added vc-starteam-locking-user so that we can tell if
;;          the current user has the file locked.
;;        - Cache the status from dired mode
;;          
;; To do:
;;    - Have Merge do an optional checkin
;;    - Integrate fully into vc-mode menus
;;
;; Known bugs:
;;    - Merge checks in the wrong file (disabled merge menu option
;;      until this is tested and fixed)
;;    - When in dired mode, some operations (getting status, locking,
;;      unlocking) switch the current buffer to the
;;      output-buffer. This should not happen, and is a side-effect of
;;      vc-starteam-actual-buffer-current-activity's reliance on
;;      'find-file when in vc-starteam-dired-mode

(eval-when-compile
  (progn
    (require 'dired)
    (require 'vc)))
(require 'dired)
(require 'vc)

(defvar vc-starteam-dired-mode-name "Dired under StarTeam" "The name of the dired mode")

(defvar vc-starteam-host "sisyphus" "*Host for  StarTeam")

(defvar vc-starteam-port 49201 "*Port on vc-starteam-host to talk to")

(defvar vc-starteam-user nil "*Who to log onto StarTeam as")

(defvar vc-starteam-locking-user nil "*The name that StarTeam uses to say that I have a file locked")

(defvar vc-starteam-password nil "password to log onto StarTeam with")

;; NOTE: if you are actually setting vc-starteam-executable on win32, the
;; value should have only 4 backslashes per backslash in the
;; path. There are 8 in the doc comments only so it shows correctly
;; when viewed via (describe-variable 'vc-starteam-executable)
(defvar vc-starteam-executable "stcmd.exe" "The name of the Starteam command-line client. 
This might be \"stcmd\" if the executable is in your path; \"/usr/local/bin/stcmd\" 
if you are on a unix platform; or \"C:\\\\\\\\Program Files\\\\\\\\StarTeam 4.0\\\\\\\\stcmd.exe\"
if you are on win32.")

(defvar vc-starteam-keep-unlocked-files-read-only t "If non-nil, files that are unlocked will be marked read-only. Mimics the \"Mark unlocked working files read-only\" option in the Win32 Starteam client's \"Workstation Options\" dialog")

(defvar vc-starteam-unlock-after-checkin t "If non-nil, files will be unlocked after they are checked in.")

(defvar vc-starteam-switch-to-output t 
  "Determines how the output of a starteam query command (such as vc-starteam-get-file-history) 
is made visible. If is t, the current buffer will be switched to the
output buffer. If nil then current buffer will remain current the
output buffer will NOT be displayed. If not t or nil then the current
buffer will remain current and the output buffer will be displayed via
'display-buffer.")

;; For debugging
(defvar vc-starteam-debug nil "If non-nil, outputs extra debugging info")

(defvar vc-starteam-to-directory-alist
  '(
    ( "^C:/starteam/view_folder" . "project/view" )
    )
  "*Map the directory to the starteam project.
The elements in the vc-starteam-to-directory-alist are of the form:

   (cons  \"^PROJECT_WORKING_FOLDER\"  \"PROJECTNAME/VIEWNAME\")

The PROJECT_WORKING_FOLDER can be found in the Win32 StarTeam
client by opening a project and choosing \"Properties\" from the
\"View\" menu; the current value in the \"Working folder\" section is
what you should use as PROJECT_WORKING_FOLDER

For example, assume that the working folder for the \"Main\" view in
the \"TopSecret\" project is set to \"C:/Starteam/TSMain\". The entry
for this looks like the following:

   (cons  \"^C:/Starteam/TSMain\"  \"TopSecret/Main\" )

The VIEWNAME is NOT hierarchical, even when the view is a child
of another view. For example, if \"Branch_A\" view is a child
of the \"Main\" view in the \"TopSecret\" project, its VIEWNAME
is \"Branch_A\", not \"Main/Branch_A\":

   (cons  \"^C:/Starteam/temp/branchA\"  \"TopSecret/Branch_A\" )

(Note: the previous example assumes that the working folder for the
Branch_A view has been set to \"C:/Starteam/temp/branchA\")
\"")

(defvar vc-starteam-post-checkin-functions nil
  "A list of functions to call after a successful checkin.
Each function is called with the arguments FILES and REASON.")




(defvar vc-starteam-menu-map (make-sparse-keymap "STARTEAM"))


(defvar vc-starteam-prop-obarray (make-vector 127 0)
  "Obarray for per-file properties.")


(defun vc-starteam-setprop (file property value)
  "Set per-file VC PROPERTY for FILE to VALUE."
  (if vc-starteam-debug (message "Setting property:%s to value:%s for file:%s" property value file))
  (vc-file-setprop file property value)
  (put (intern file vc-starteam-prop-obarray) property value))

(defun vc-starteam-getprop (file property)
  "Get per-file VC PROPERTY for FILE."
  (if vc-starteam-debug (message "Getting property:%s for file:%s" property file))
  (get (intern file vc-starteam-prop-obarray) property))

;****************************************
;                                        
;  vc-starteam-registered 
;
;  Determine if the the file is regestered with StarTeam.  The file is
;  registered if any of these conditions hold:
;
; 1 - The vc-state property exists and is not 'missing
; 2 - StarTeam is responsible and the state  is not nil or 'missing
;                                        
;****************************************
(defun vc-starteam-registered (file)
  "Check to see if a file is to be used under StarTeam"
  (interactive "fFile Name:")
  (if vc-starteam-debug (message "vc-starteam-registered %s" file))
  (let ((state (vc-starteam-getprop file 'vc-state)))
	(if (equal state 'missing) nil
	  (if  state  t
		(if (vc-starteam-responsible-p file)
			(progn
			  (setq state (vc-starteam-state file))
			  (if (equal state 'missing) nil state))
		  nil)))))
	  

;****************************************
;                                        
;  vc-starteam-responsible-p
;                                        
;****************************************
(defun vc-starteam-responsible-p (ufile)
"Determine if this file is in StarTeam. See \\[vc-starteam-get-vc-starteam-path-from-local-path] for more info"
  (interactive "fFile Name:")
  (if vc-starteam-debug (message "vc-starteam-responsible-p %s" ufile))
  (if (string-match dired-omit-files (file-name-nondirectory ufile))
	  nil
	(let* ((dir (file-name-directory (expand-file-name ufile)))
		   (starteam-path	(vc-starteam-get-vc-starteam-path-from-local-path dir t)))
	  starteam-path)))


(defun vc-starteam-dir-state ( udir )
  "Get the state of each file in UDIR.  Set the following properties:
   vc-backend
   vc-state

  "
  (interactive "DDir Name:")
  (let* ((command "list")
		 (dired-buffer (current-buffer))
		 (missing-files)
		 (fullpath (expand-file-name udir))
		 (fullpath-regexp (replace-regexp-in-string "[/\\]" "[/\\\\]"
													(directory-file-name fullpath)))
		 (dir  fullpath)
		 (default-directory dir)
		 (tempdir)
		 (file "*")
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
		 (string-state "Unknown"))
	;;(message "Checking status of directory %s from buffer %s ..." fullpath (current-buffer))
	(if (not (local-variable-p 'vc-dir-state-output-buffer))
		(progn
		  (make-local-variable 'vc-dir-state-output-buffer)
		  (make-local-variable 'file-name-handler-alist)
		  
		  (save-excursion
			;;(message "Fetching state of %s" fullpath)
			(setq vc-dir-state-output-buffer
				  (vc-starteam-execute nil command 
									   "GET FILE STATUS" path file "-cf" "-is"
									   "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
			(set-buffer vc-dir-state-output-buffer)
			(goto-char (point-min))

			(while (not (eobp))
			  (let* (( next-line-add-newlines nil)
					 (tempfile)
					 (tempfile-start-point)
					 (default-directory)
					 ( line-start (progn (forward-line 0) (point)))
					 ( line-end   (progn (end-of-line) (point)))
					 ( line-text  (buffer-substring line-start line-end)))
				(if (string-match "^Folder:\\s-*.*dir:\\s-\\(.*\\))$" line-text)
					(save-excursion
					  (setq tempdir (substring  line-text (match-beginning 1) 
												(match-end 1)))
					  (setq default-directory tempdir)
					  (set-buffer dired-buffer)
					  (if (dired-goto-subdir tempdir) 
						  nil
						(progn										
						  (message "adding mama %s" tempdir)
						  (goto-char (point-min))
						  (insert "  " tempdir ":\n\n")))))
				
				(if (string-match "^Missing"
								  line-text)
					(save-excursion
					  (setq tempfile-start-point 
							(progn 
							  (move-to-column 63) 
							  (point)))
					  (setq tempfile 
							(progn
							  (end-of-line) 
							  (expand-file-name 
							   (buffer-substring-no-properties 
								tempfile-start-point 
								(point)))))
					  (set-buffer dired-buffer)
					  (goto-char (point-min))
					  (message "adding file %s%s"  tempdir tempfile)
					  (setq file-name-handler-alist (append file-name-handler-alist
															 (list (cons
															  (concat tempdir tempfile)
															  'vc-starteam-missing-file-name-handler))))
					  (message "handler alist: %s" file-name-handler-alist)
					  (vc-starteam-setprop (concat tempdir tempfile) 'vc-backend 'STARTEAM)
					  (vc-starteam-setprop (concat tempdir tempfile) 'vc-state 'needs-patch)
					  (dired-add-entry  
					   (concat tempdir tempfile) nil t)))
			  (forward-line 1)))
			(save-excursion
			  (set-buffer dired-buffer)
			  (dired-build-subdir-alist)))))
	(save-excursion
	  (set-buffer vc-dir-state-output-buffer)
	  (goto-char (point-min))
	  (if (re-search-forward fullpath-regexp nil t)
		  (let ((start (point))
				(end (point-max)))
			(if (re-search-forward "^Folder:" nil t) (setq end (point)))
			(goto-char start)
			(while (re-search-forward "^\\(Out of Date\\|Unknown\\|Current\\|\\Modified\\|\\Missing\\|Merge\\|Not in View\\)" end t)
										; return the matched string
			  (progn
				(setq string-state (buffer-substring-no-properties 
									(match-beginning 1) (match-end 1)))
				(setq locking-user (let* ((start (progn (move-to-column 12) 
														(point)))
										  (end (progn (move-to-column 26)
													  (point)))
										  (text (buffer-substring-no-properties start end)))
									 (message "Looking for lock in [%s]" text)
									 (if (string-match "^ *\\([^ ].*?\\) *$" text)
										 (progn
										   (message "Locking user:[%s]"
										   (substring text (match-beginning 1)
													  (match-end 1)))
										   (substring text (match-beginning 1)
													  (match-end 1)))
									   nil)))
				(setq state (cond (locking-user 
								   
								   (if (string-equal locking-user vc-starteam-locking-user)
									   'edited locking-user))
								  ((string-equal "Merge" string-state) 'needs-merge)
								  ((string-equal "Out of Date" string-state) 'needs-patch)
								  ((string-equal "Unknown" string-state) nil)
								  ((string-equal "Current" string-state) 'up-to-date)
								  ((string-equal "Modified" string-state) 'edited)
								  ((string-equal "Missing" string-state) 'needs-patch)
								  ((string-equal "Not in View" string-state) 'missing)))
				(if state 
					(let* ((p (progn (move-to-column 63) (point)))
						   (default-directory fullpath)
						   (file (progn (end-of-line) (expand-file-name (buffer-substring-no-properties p (point))))))

					  (vc-starteam-setprop file 'vc-backend 'STARTEAM)
					  (vc-starteam-setprop file 'vc-state state)
					  (message "vc-starteam-setprop %s 'vc-state %s" file state))
					  ;;(message "State: %s %s" file state))
				  (end-of-line)))))))))
  

(defun vc-starteam-dir-state-new ( udir )
  "Get the state of each file in UDIR.  Set the following properties:
   vc-backend
   vc-state

  "
  (interactive "DDir Name:")
  (let* ((command "hist")
		 (dired-buffer (current-buffer))
		 (missing-files)
		 (fullpath (expand-file-name udir))
		 (fullpath-regexp (replace-regexp-in-string "[/\\]" "[/\\\\]"
													(directory-file-name fullpath)))
		 (dir  fullpath)
		 (default-directory dir)
		 (tempdir)
		 (file "*")
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
		 (string-state "Unknown"))
	;;(message "Checking status of directory %s from buffer %s ..." fullpath (current-buffer))
	(if (not (local-variable-p 'vc-dir-state-output-buffer))
		(progn
		  (make-local-variable 'vc-dir-state-output-buffer)
		  (make-local-variable 'file-name-handler-alist)
		  
		  (save-excursion
			;;(message "Fetching state of %s" fullpath)
			(setq vc-dir-state-output-buffer
				  (vc-starteam-execute nil command 
									   "GET FILE STATUS USING HIST" path file 
									   "-is"
									   "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
			(set-buffer vc-dir-state-output-buffer)
			(goto-char (point-min))

			(while (not (eobp))
			  (let* (( next-line-add-newlines nil)
					 (tempfile)
					 (tempfile-start-point)
					 (default-directory)
					 ( line-start (progn (forward-line 0) (point)))
					 ( line-end   (progn (end-of-line) (point)))
					 ( line-text  (buffer-substring line-start line-end)))
				(if (string-match "^Folder:\\s-*.*dir:\\s-\\(.*\\))$" line-text)
					(save-excursion
					  (setq tempdir (substring  line-text (match-beginning 1) 
												(match-end 1)))
					  (setq default-directory tempdir)
					  (set-buffer dired-buffer)
					  (if (dired-goto-subdir tempdir) 
						  nil
						(progn										
						  (message "adding mama %s" tempdir)
						  (goto-char (point-min))
						  (insert "  " tempdir ":\n\n")))))
				
				(if (string-match "^Missing"
								  line-text)
					(save-excursion
					  (setq tempfile-start-point 
							(progn 
							  (move-to-column 63) 
							  (point)))
					  (setq tempfile 
							(progn
							  (end-of-line) 
							  (expand-file-name 
							   (buffer-substring-no-properties 
								tempfile-start-point 
								(point)))))
					  (set-buffer dired-buffer)
					  (goto-char (point-min))
					  (message "adding file %s%s"  tempdir tempfile)
					  (setq file-name-handler-alist (append file-name-handler-alist
															 (list (cons
															  (concat tempdir tempfile)
															  'vc-starteam-missing-file-name-handler))))
					  (message "handler alist: %s" file-name-handler-alist)
					  (vc-starteam-setprop (concat tempdir tempfile) 'vc-backend 'STARTEAM)
					  (vc-starteam-setprop (concat tempdir tempfile) 'vc-state 'needs-patch)
					  (dired-add-entry  
					   (concat tempdir tempfile) nil t)))
			  (forward-line 1)))
			(save-excursion
			  (set-buffer dired-buffer)
			  (dired-build-subdir-alist)))))
	(save-excursion
	  (set-buffer vc-dir-state-output-buffer)
	  (goto-char (point-min))
	  (if (re-search-forward fullpath-regexp nil t)
		  (let ((start (point))
				(end (point-max)))
			(if (re-search-forward "^Folder:" nil t) (setq end (point)))
			(goto-char start)
			(while (re-search-forward "^\\(Out of Date\\|Unknown\\|Current\\|\\Modified\\|\\Missing\\|Merge\\|Not in View\\)" end t)
										; return the matched string
			  (progn
				(setq string-state (buffer-substring-no-properties 
									(match-beginning 1) (match-end 1)))
				(setq locking-user (let* ((start (progn (move-to-column 12) 
														(point)))
										  (end (progn (move-to-column 26)
													  (point)))
										  (text (buffer-substring-no-properties start end)))
									 (message "Looking for lock in [%s]" text)
									 (if (string-match "^ *\\([^ ].*?\\) *$" text)
										 (progn
										   (message "Locking user:[%s]"
										   (substring text (match-beginning 1)
													  (match-end 1)))
										   (substring text (match-beginning 1)
													  (match-end 1)))
									   nil)))
				(setq state (cond (locking-user 
								   
								   (if (string-equal locking-user vc-starteam-locking-user)
									   'edited locking-user))
								  ((string-equal "Merge" string-state) 'needs-merge)
								  ((string-equal "Out of Date" string-state) 'needs-patch)
								  ((string-equal "Unknown" string-state) 'missing)
								  ((string-equal "Current" string-state) 'up-to-date)
								  ((string-equal "Modified" string-state) 'edited)
								  ((string-equal "Missing" string-state) 'needs-patch)
								  ((string-equal "Not in View" string-state) 'missing)))
				(if state 
					(let* ((p (progn (move-to-column 63) (point)))
						   (default-directory fullpath)
						   (file (progn (end-of-line) (expand-file-name (buffer-substring-no-properties p (point))))))

					  (vc-starteam-setprop file 'vc-backend 'STARTEAM)
					  (vc-starteam-setprop file 'vc-state state)
					  (message "vc-starteam-setprop %s 'vc-state %s" file state))
					  ;;(message "State: %s %s" file state))
				  (end-of-line)))))))))
  



(defun vc-starteam-missing-file-name-handler (operation &rest args )
  (message "Perfrom %s on %s" operation args)
  (cond ((eq operation 'insert-directory) (insert (concat "-rw-rw-r--    1 lpmsmith lpmsmith     3349 May 29 13:35 " (file-name-nondirectory (car args) )"\n" )))
		 (t (vc-starteam-run-real-handler operation args))))


(defun vc-starteam-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, remaining ARGS are passed to the
OPERATION."
  (let ((inhibit-file-name-handlers
         (list 'vc-starteam-missing-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;****************************************
;                                        
;  vc-starteam-register
;                                        
;****************************************
(defun vc-starteam-register (ufile &optional rev comment)
  "Add the file in the current buffer"
  (interactive)

  (let* ((command "add")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (output-buffer)
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir)))

    (save-excursion 
      (message "Adding in file %s%s ..." dir file)

	  
	  (setq output-buffer 
			(vc-starteam-execute nil command 
								   "ADD FILE" path file
								   "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
	  (vc-starteam-setprop (concat dir file) 'vc-state 'up-to-date)
)))

(defun vc-starteam-revert (ufile &optional contents-done)
  " Revert FILE back to the current workfile version.  If optional
   arg CONTENTS-DONE is non-nil, then the contents of FILE have
   already been reverted from a version backup, and this function
   only needs to update the status of FILE within the backend."
  (interactive)

  (unless contents-done
	(let* ((command "co")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (output-buffer)
		 (must-make-file-read-only vc-starteam-keep-unlocked-files-read-only)
		 (unlock-operation (if must-make-file-read-only "-u" "-l"))
		 (read-write-operation (if must-make-file-read-only "-ro" "-rw"))
		 (forced "-o")
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir)))

    (save-excursion 
      (message "Reverting file %s%s ..." dir file)

	  (setq output-buffer 
			(vc-starteam-execute nil command 
								   "REVERT FILE" path file
								   unlock-operation
								   read-write-operation
								   forced
								   "-rp" (vc-starteam-get-working-dir-from-local-path dir)))))))



(defun vc-starteam-merge (ufile first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (interactive)

  (let* ((command "co")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (output-buffer)
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir)))

    (save-excursion 
      (message "Merging file %s%s ..." dir file)

	  (setq output-buffer 
			(vc-starteam-execute nil command 
								   "ADD FILE" path file
								   "-merge"
								   "-vn" first-version
								   "-rp" (vc-starteam-get-working-dir-from-local-path dir))))))



(defun vc-starteam-merge-news (ufile)
  "Merge changes into current working copy of FILE."

  (interactive)

  (let* ((command "co")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (output-buffer)
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir)))

    (save-excursion 
      (message "Merging file %s%s ..." dir file)

	  (setq output-buffer 
			(vc-starteam-execute nil command 
								   "ADD FILE" path file
								   "-merge"
								   "-rp" (vc-starteam-get-working-dir-from-local-path dir))))))


(defun vc-starteam-state-heuristic (ufile)
  (if vc-starteam-debug (message "vc-starteam-state-heuristic %s" ufile))
  (let ((state (vc-starteam-getprop ufile 'vc-state)))
	(if state state 'up-to-date )
))

;****************************************
;                                        
;  vc-starteam-state
;                                        
;****************************************
(defun vc-starteam-state (ufile)
  "Determine the state of the file.  See vc-state for more info"
  (let* ((command "hist")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
		 (string-state "Unknown")
		 (locking-user)
		 (output-buffer))

    (message "Checking status of %s ..." fullpath)
	(if  (local-variable-p 'vc-dir-state-output-buffer)
		(vc-starteam-dir-state dir)
	  (save-excursion
		(setq output-buffer 
			  (vc-starteam-execute nil command 
								   (concat "GET FILE STATUS using " command)
								   path file
								   "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
		
		(set-buffer output-buffer)
		(goto-char (point-min))      
		;; Figure out who is locking the file
		(if (re-search-forward "^Locked by: \\([a-zA-Z].*\\)$" nil t)
			(progn
			  (setq locking-user (buffer-substring-no-properties 
								  (match-beginning 1) (match-end 1)))
			  (if vc-starteam-debug (message "Locked by=%s" locking-user))))

		;; Get the status
		(if (re-search-forward "^Status: \\(Out of Date\\|Unknown\\|Current\\|\\Modified\\|\\Missing\\|Merge\\|Not in View\\)" nil t)
										; return the matched string
			(let (rtnval)
			  (setq string-state (buffer-substring-no-properties 
								  (match-beginning 1) (match-end 1)))
								   
			  (setq rtnval 
					(cond  
					 ((and locking-user (not (string-equal locking-user 
										 vc-starteam-locking-user)))
					  locking-user)
					 ((and locking-user (string-equal locking-user 
										 vc-starteam-locking-user))
					  'edited)
					 ((string-equal "Merge" string-state) 'needs-merge)
					 ((string-equal "Out of Date" string-state) 'needs-patch)
					 ((string-equal "Unknown" string-state) 'missing)
					 ((string-equal "Current" string-state) 'up-to-date)
					 ((string-equal "Modified" string-state) 'edited)
					 ((string-equal "Missing" string-state) 'needs-patch)
					 ((string-equal "Not in View" string-state) 'missing)))
			  (vc-starteam-setprop fullpath 'vc-state rtnval)
			  (if vc-starteam-debug (message "Status=%s" rtnval))
			  (if (re-search-forward "^Revision: \\([0-9][0-9]*\\)" nil t)
				  (let ((revision))
					(setq revision (buffer-substring-no-properties 
									(match-beginning 1) (match-end 1)))
					(vc-starteam-setprop file 'vc-workfile-version revision)
					(if vc-starteam-debug (message "Revision=%s" revision))))
			  rtnval))))))
;****************************************
;                                        
;  vc-starteam-print-log
;                                        
;****************************************
(defun vc-starteam-print-log (ufile)
  "Get the revision history log of FILE"
  (interactive "fFile Name:")
  (let* ((command "hist")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
		 (output-buffer))
    
    (message "Getting history of %s%s ..." dir file)
    (setq output-buffer (vc-starteam-execute "*vc*" command "GET FILE HISTORY" path file 
					  "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
    (vc-starteam-display-buffer output-buffer)
    (message "History placed in buffer <%s>" output-buffer)
    ))


(defun vc-starteam-diff (ufile &optional rev1 rev2)
  "Get the diff of FILE"
  (interactive "fFile Name:")
  (let* ((command "diff")
		 (fullpath (expand-file-name ufile))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
		 (output-buffer))
    
    (message "Getting %s of %s%s ..." command dir file)
    (setq output-buffer (vc-starteam-execute "*vc-diff*" command "GET FILE DIFF" path file 
					  (if rev1 "-vn")(if rev1 rev1)						 
					  (if rev2 "-vn")(if rev2 rev2)						 
					  "-c" "5"
					  "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
    ;;(vc-starteam-display-buffer output-buffer)
    (message "%s placed in buffer <%s>" command output-buffer)
	(save-excursion
	  (set-buffer output-buffer)
	  (goto-char (point-min))
	  (if (re-search-forward "^Revisions are identical" nil t)
		  0 1))))


(defun vc-starteam-workfile-version (file)
  "Get the working revision of FILE"
  (let ((buf "*vc-diff*")
        (visiting-buffer (find-buffer-visiting file))
		(stored-rev (vc-starteam-getprop file 'vc-workfile-version))
		(rev))
	;; Only do anything if we are responsible for this file
	(if (vc-starteam-responsible-p file)
		;; If we already know the revision number, use it
		(if stored-rev stored-rev
		  ;; Check to see if the revision number is in expanded format
		  (save-excursion
			(if vc-starteam-debug (message "Buffer:%s is visiting file:%s" visiting-buffer file))
			(set-buffer visiting-buffer)
			(goto-char(point-min))
			(if (re-search-forward "[$]Revision:\\s-*\\([0-9]+\\)[$]" nil t)
				(setq rev (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
			  ;; Otherwise get the diff of the file and find the version number
			  (save-excursion
				(vc-starteam-diff file)
				(set-buffer buf)
				(goto-char(point-min))
				;; Get the revision number
				(if (re-search-forward "Revision:\\s-+\\([0-9]+\\)" nil t)
					(setq rev (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
				  (error "Error checking status; see buffer %s" buf))))
			(vc-starteam-setprop file 'vc-workfile-version rev)
		rev))
	  nil)))
		  
		

(defun vc-starteam-checkout-model (file)
  'locking)

(defun vc-starteam-get-user ()
  "Get the name of the user for StarTeam"
  (interactive)
  (if vc-starteam-user vc-starteam-user
    (progn
      (setq vc-starteam-user (read-string "Enter user name: " (user-login-name)))
      vc-starteam-user)))
 

(defun vc-starteam-get-password ()
  "Get the password for the user of StarTeam"
  (interactive)
  (if vc-starteam-password vc-starteam-password
      (setq vc-starteam-password (read-passwd (concat "Enter password for user "
					       (vc-starteam-get-user)
					       ": ")))))

(defun vc-starteam-get-login-info ()
  "Get the login information for the starteam server.

Returns a string in the form:

      USER:PASSWORD@HOST_MACHINE:PORT"
  (concat (vc-starteam-get-user) ":"
	  (vc-starteam-get-password) "@"
	  vc-starteam-host ":" vc-starteam-port))


(defun vc-starteam-check-file-status ()
  "Checks the status of the file in the current buffer
and returns the buffer containing the output of the status check"
  (interactive)
  (let* ((command "list")
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (output-buffer))

    (message "Checking status of %s%s ..." dir file)

    (save-excursion
      (setq output-buffer 
	    (vc-starteam-execute nil command 
			      "GET FILE STATUS" path file
			      "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
      )
    output-buffer 
    ))

(defun vc-starteam-get-file-status-as-string ()
  "Calls vc-starteam-check-file-status, then searches the status buffer 
and returns the file status as one of the following strings:

    Out of Date
    Unknown
    Current
    Modified
    Missing
    Merge
    Not in View"
  (interactive)
  (let* ((status-string)
	(status-buffer (vc-starteam-check-file-status)))

    (save-excursion
      (set-buffer status-buffer)
      (goto-char (point-min))      
      (if (re-search-forward "^\\(Out of Date\\|Unknown\\|Current\\|\\Modified\\|\\Missing\\|Merge\\|Not in View\\)" nil t)
	  ; return the matched string
	  (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	(error "Error checking status; see buffer %s" status-buffer))
      )
    ))

(defun vc-starteam-checkout-file (&optional force)
  "Checkout the file in the current buffer

force - if non-nil, forces the checkout"
  (interactive)
  (let* ((command "co")
	 (buf (vc-starteam-actual-buffer-current-activity))
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (file (cadr dir-and-file))	 
	 ;; if this file is missing from the working directory and
	 ;; vc-starteam-keep-unlocked-files-read-only is non-nil, then
	 ;; we'll explicity tell the starteam command to set the newly
	 ;; checked-out file to be read-only (because by default the
	 ;; starteam command-line client sets newly checked-out files
	 ;; to read-write). We also have to unlock the file since
	 ;; starteam doesn't allow setting read-only outside of the
	 ;; context of a lock or unlock operation. Jeesh!
	 (must-make-file-read-only (if (and vc-starteam-keep-unlocked-files-read-only 
					    (vc-starteam-is-file-locked-by-user))
				       t
				     nil))
	 (unlock-operation (if must-make-file-read-only "-u" nil))
	 (read-write-operation (if must-make-file-read-only "-ro" nil))
	 (output-buffer)
	 )
    
    (save-excursion 
      (set-buffer buf)
      (save-buffer))
    
    (message "Checking out file %s%s ..." dir file)

    (save-excursion
      (setq output-buffer (vc-starteam-execute nil command "CHECK OUT FILE" path file 
					    read-write-operation unlock-operation 
					    "-rp"
					    (vc-starteam-get-working-dir-from-local-path dir)
					    (if force "-o" nil) ; force checkout
					    ))
      (if (not (vc-starteam-output-buffer-contains output-buffer (format "^%s: checked out" file)))

	  ;; local file is different from starteam file; confirm the checkout
	  (let ((file-status (vc-starteam-get-file-status-as-string)))
	    (if (yes-or-no-p (format "Status of %s%s is: %s. Force checkout of file?" 
				     dir file file-status))
		;; do a forced checkout
		(vc-starteam-checkout-file t) 

	      ;; remind user that the checkout was aborted
	      (error "ABORTED checkout of %s%s because its status is: %s" dir file file-status) 
	      ))
	
	;; checkout was successful
	(set-buffer buf)
	(message "Reverting buffer %s ..." (current-buffer))
	(revert-buffer t t)
	(message "Checked out file %s%s ..." dir file)
	))
    ))



(defun vc-starteam-checkout-dir-recursive ( udir ) 
  "Checkout the current directory and all subdirectories.  Review the results in dired mode"               
  (interactive "DDirectory Name:")
  (let* ((command "co")               
         (buf (current-buffer)) 
         (dir (expand-file-name udir)) 
	 (tempdir nil)
         (path (vc-starteam-get-vc-starteam-path-from-local-path 
		dir)) 
         (output-buffer) 
         ) 
         
    (message "Checking out dir %s ..." dir ) 
        
    (save-excursion 
      (setq output-buffer (vc-starteam-execute nil command 
					    "CHECK OUT FILE" path  
                                            "-is" 
                                            "-rp" 
                                            (vc-starteam-get-working-dir-from-local-path 
					     dir) 
                                            )) 
       
      (message "Checked out dir %s ..." dir) 
      (switch-to-buffer-other-window output-buffer t)
      (delete-matching-lines ": skipped$")
      (goto-char (point-min))
      ;;
      ;; Go through each line and determine what the file staus is
      ;;
      (while (not (eobp))
	(progn
	  (let* (( next-line-add-newlines nil)
		 ( line-start (progn (forward-line 0) (point)))
		 ( line-end   (progn (end-of-line) (point)))
		 ( line-text  (buffer-substring line-start line-end))
		 )
	    (if (string-match "^Folder:\\s-*.*dir:\\s-\\(.*\\))$" line-text)
		(progn
		  (setq tempdir (substring  line-text (match-beginning 1) (match-end 1)))
		  (message "dir:%s" tempdir)
		  (delete-region line-start line-end))
	      (if (string-match "\\(.*\\):\\s-file status is Merge,.*" line-text)
		  (progn
		    (setq file-name (substring  line-text 
						    (match-beginning 1) 
						    (match-end 1)))
		    (delete-region line-start line-end)
		    (insert  "Merge       " )
		    (insert-directory (concat tempdir "/" file-name) "-l"))
		(if (string-match "\\(.*\\):\\s-checked out.*" line-text)
		    (progn
		      (setq file-name (substring  line-text 
						  (match-beginning 1) 
						  (match-end 1)))
		      (delete-region line-start line-end)
		      (insert  "Up to Date " )
		      (insert-directory (concat tempdir "/" file-name) "-l"))
		  (if (string-match "\\(.*\\):\\s-file status is Modified.*" line-text)
		      (progn
			(setq file-name (substring  line-text 
						    (match-beginning 1) 
						    (match-end 1)))
			(delete-region line-start line-end)
			(insert  "Modified   " )
			(insert-directory (concat tempdir "/" file-name) "-l"))

			
	      (progn
		(delete-blank-lines)
		(message "NOT:%s" line-text))))))
	  
	  (forward-line 1))))
      (goto-char (point-min))
      (delete-blank-lines)
      (while (not (eobp))
	(progn
	  (forward-line 1)
	  (delete-blank-lines)))
	  
      (vc-starteam-dired-mode))))


(defun vc-starteam-checkout (ufile &optional editable rev destfile)
  "Checkout the FILE"
  (interactive)
  (let* ((command "co")
	 ;;(buf (vc-starteam-actual-buffer-current-activity))
	 ;;(dir-and-file (vc-starteam-current-buffer-dir-and-file))
		 (fullpath (expand-file-name ufile))
		 (temp-directory (concat temporary-file-directory "/StarTeamTemp-" user-full-name))
		 (dir (file-name-directory fullpath))
		 (file (file-name-nondirectory fullpath))
		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))

	 ;; if this file is missing from the working directory and
	 ;; vc-starteam-keep-unlocked-files-read-only is non-nil, then
	 ;; we'll explicity tell the starteam command to set the newly
	 ;; checked-out file to be read-only (because by default the
	 ;; starteam command-line client sets newly checked-out files
	 ;; to read-write). We also have to unlock the file since
	 ;; starteam doesn't allow setting read-only outside of the
	 ;; context of a lock or unlock operation. Jeesh!
	 (must-make-file-read-only (not editable))
	 (unlock-operation (if must-make-file-read-only "-u" "-l"))
	 (read-write-operation (if must-make-file-read-only "-ro" "-rw"))
	 (forced (if must-make-file-read-only nil "-o"))
	 (output-buffer)
	 (tdir)
	 )
    
    (message "Checking out file %s%s rev [%s] into %s ..." path file rev temp-directory)

    (save-excursion
      (setq output-buffer (vc-starteam-execute nil command "CHECK OUT FILE" path file 
					    read-write-operation unlock-operation forced
					    "-rp" (if destfile temp-directory 
								(vc-starteam-get-working-dir-from-local-path dir))
						(if (and rev (string< "" rev))  "-vn")
						(if (and rev (string< "" rev)) rev)
					    ;;(if force "-o" nil) ; force checkout
					    ))
	  (set-buffer output-buffer)
	  (goto-char (point-min))
	  (if (re-search-forward "working dir:\\s-+\\([^)]+\\))" nil t)
		  (setq tdir (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
		(error "Error checking status; see buffer %s" output-buffer))
	  (if destfile (rename-file (concat tdir "/" file) destfile t)))
	(message "Checking out file %s%s rev [%s] into %s ... DONE" path file rev temp-directory)))





;; (defun vc-starteam-checkout (ufile &optional editable rev destfile)
;;   "Used to integrate with VC mode.  Checkout a file"
;;   (let* ((command "co")
;; 		 (temp-directory (concat temporary-file-directory "/StarTeamTemp-" user-full-name))
;; 		 (file (expand-file-name ufile))
;; 		 (dir (file-name-directory file))
;; 		 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
;; 		 (output-buffer))
  
;;     ;;(message "Checking out temporary version of file %s%s to %s/%s%s" 
;; 	;;     dir file temp-directory dir file)
;;     (save-excursion
;;       (setq output-buffer (vc-starteam-execute nil command "TEMPORARY CHECK OUT OF FILE" path file 
;; 				    "-rp" temp-directory))
;;       (goto-char (point-min))
;;       (if (re-search-forward "(working dir: \\(.*\\))" nil t)
;; 	  (concat (buffer-substring-no-properties (match-beginning 1)
;; 						  (match-end 1))
;; 		  "/" file)
;; 	nil)
;;       )
;;     ))


(defun vc-starteam-checkout-temp-file ()
  "Checkout the file in the current buffer into a temp dir.  Return the path name of the temp file"
  (interactive)
  (let* ((command "co")
	(temp-directory (concat temporary-file-directory "/StarTeamTemp-" user-full-name))
	(dir (file-name-directory (buffer-file-name)))
	(file (buffer-name))
	(path (vc-starteam-get-vc-starteam-path-from-local-path dir)))

    (save-buffer)
    (message "Checking out temporary version of file %s%s to %s/%s%s" 
	     dir file temp-directory dir file)
    (save-excursion
      (set-buffer (vc-starteam-execute nil command "TEMPORARY CHECK OUT OF FILE" path file 
				    "-rp" temp-directory))
      (goto-char (point-min))
      (if (re-search-forward "(working dir: \\(.*\\))" nil t)
	  (concat (buffer-substring-no-properties (match-beginning 1)
						  (match-end 1))
		  "/" file)
	nil)
      )
    ))

;; maybe should have vc-starteam-check-file-status return vector of all
;; status elements? Then this method could simply check to see if the
;; file is locked by the current user.
(defun vc-starteam-is-file-locked-by-user ()
  (interactive)
  
  (string-equal (vc-starteam-check-file-status)  "Missing")
)

;;
;; Used by VC to checkin the file
;;

(defun vc-starteam-checkin ( file rev comment )
  (let* ((reason comment)
		 (force (if (boundp 'vc-starteam-force-checkin) vc-starteam-force-checkin nil))
		 ;; (buf vc-starteam-last-buffer)
		 (files (list file))
		 (command "ci")
		 (unlock-operation (if vc-starteam-unlock-after-checkin "-u" nil)) 
		 (read-write-operation-for-checkin (if (and vc-starteam-unlock-after-checkin vc-starteam-keep-unlocked-files-read-only)
											   "-ro" 
											 nil)) 
		 (path nil)
		 (file nil))
    
    ;;(save-buffer)
    ;(message "Checking in file %s%s ..." dir file)
    
    ;****************************************
    ;                                        
    ;  Iterate over all the files, checking in each one
    ;                                        
    ;****************************************
    (mapcar 
	 (lambda (fullpath)
	   (setq path (vc-starteam-get-vc-starteam-path-from-local-path (file-name-directory fullpath)))
	   (setq file (file-name-nondirectory fullpath))
	   (let ((operation-success-magic-text  "checked in")
			 (output-buffer))
		 (if path
			 (progn
			   (save-excursion
				 (setq output-buffer 
					   (vc-starteam-execute nil command command path file 
											"-r" (concat "\"" reason "\"")
											"-rp" 
											(vc-starteam-get-working-dir-from-local-path (file-name-directory fullpath))
											(if force "-o" nil)
											unlock-operation 
											read-write-operation-for-checkin))
				 ;; check operation output buffer for errors
				 (message "Checking <%s> for \"%s\"" 
						  output-buffer 
						  (format "^%s: %s" file operation-success-magic-text))
				 (if (not (vc-starteam-output-buffer-contains output-buffer (format "^%s: %s" file operation-success-magic-text)))			    
					 ;; operation failed. Abort with explanation.
					 (error "ABORTED. %s%s was NOT %s. See buffer <%s> for more details." 
							(file-name-directory fullpath) file operation-success-magic-text output-buffer)
				   ;; operation was successful
				   (message "%s%s successfully %s" (file-name-directory fullpath) file operation-success-magic-text)
				   ;; Reset the workfile-version
				   (vc-starteam-setprop (concat (file-name-directory fullpath) file) 'vc-workfile-version nil)
				   )
				 
				 ) ; end save-excursion		      		     
			   )))
	   file) 
	 files)
    (mapcar (lambda (x)
	      (message "%s" x)
	      (funcall x files reason))
	    vc-starteam-post-checkin-functions)
    (revert-buffer nil t)	  
    (delete-window)
    ))


(defun vc-starteam-finish-logentry ()
  "Do the actual checking in or adding of files once they have had their log entry set."
  (interactive)
  (let* ((reason (buffer-string))
	 (force (if (boundp 'vc-starteam-force-checkin) vc-starteam-force-checkin nil))
	 (buf vc-starteam-last-buffer)
	 (files vc-starteam-checkin-files)
	 (command vc-starteam-command)
	 (unlock-operation (if vc-starteam-unlock-after-checkin "-u" nil)) 
	 (read-write-operation-for-checkin (if (and vc-starteam-unlock-after-checkin vc-starteam-keep-unlocked-files-read-only)
					       "-ro" 
					     nil)) 
	 (read-write-operation-for-add (if vc-starteam-keep-unlocked-files-read-only "-ro" nil)) 
	 (path nil)
	 (file nil))
    
    ;;(save-buffer)
    ;(message "Checking in file %s%s ..." dir file)
    
    ;****************************************
    ;                                        
    ;  Iterate over all the files, checking in each one
    ;                                        
    ;****************************************
    (mapcar (lambda (fullpath)
	      (setq path (vc-starteam-get-vc-starteam-path-from-local-path (file-name-directory fullpath)))
	      (setq file (file-name-nondirectory fullpath))
	      (let ((operation-success-magic-text (cond ((string-equal command "ci") "checked in")
							((string-equal command "add") "added")
							(t (error "ERROR: unknown command \"%s\" sent to vc-starteam-finish-logentry" command))
							))
		    (output-buffer))
		(if path
		    (progn
		      (save-excursion
			(cond (
			       ;; CHECKING IN an existing file
			       (string-equal command "ci")			     
			       (setq output-buffer 
				     (vc-starteam-execute nil command command path file 
						       "-r" reason
						       "-rp" 
						       (vc-starteam-get-working-dir-from-local-path (file-name-directory fullpath))
						       (if force "-o" nil)
						       unlock-operation 
						       read-write-operation-for-checkin
						       ))				 
			       )

			      ;; ADDING a new file
			      ((string-equal command "add")
			       (message "Adding file %s%s to Starteam path %s" (file-name-directory fullpath) file path)			     
			       (setq output-buffer (vc-starteam-execute nil command command path file 
								     "-rp" (vc-starteam-get-working-dir-from-local-path (file-name-directory fullpath)) 
								     "-u" read-write-operation-for-add "-d" reason))			       
			       )

			      ;; UNKNOWN command
			      ;; (already trapped this error in definition of operation-success-magic-text

			      ) ; end 'cond
			
			;; check operation output buffer for errors
			(message "Checking <%s> for \"%s\"" output-buffer (format "^%s: %s" file operation-success-magic-text))
			(if (not (vc-starteam-output-buffer-contains output-buffer (format "^%s: %s" file operation-success-magic-text)))			    
			    ;; operation failed. Abort with explanation.
			    (error "ABORTED. %s%s was NOT %s. See buffer <%s> for more details." 
				   (file-name-directory fullpath) file operation-success-magic-text output-buffer)
			  ;; operation was successful
			  (message "%s%s successfully %s" (file-name-directory fullpath) file operation-success-magic-text)
			  )

			) ; end save-excursion		      		     
		      )))
	      file) 
	    files)
    (mapcar (lambda (x)
	      (message "%s" x)
	      (funcall x files reason))
	    vc-starteam-post-checkin-functions)
    (switch-to-buffer buf)
    (revert-buffer nil t)	  
    (delete-window)
    ))

;****************************************
;                                        
;  Check in multiple files
;                                        
;****************************************
(defun vc-starteam-checkin-multiple-files (files &optional force)
  "Checkin all the files in 'files' with a single reason.

files - a list of fully qualified files to check in
force - if non-nil, forces the checkins"
  (interactive "fFile to check in: ")
  (let* ((f (if (listp files) files (list files)))
	 (path nil)
	 (command "ci")
	 (listbuf (get-buffer-create "*Check-in-List*")))
    (set-buffer listbuf)
    (erase-buffer)
    (insert-string "The following files are selected to check in:\n")
    (mapcar (lambda (x) (insert x) (insert "\n")) f)
    (pop-to-buffer listbuf)
    (shrink-window-if-larger-than-buffer)
    (vc-starteam-log-mode command f (current-buffer) force)
    ))

(defun vc-starteam-checkin-file (&optional force)
  "Checkin the file in the current buffer

force:
   if non-nil, forces the checkin "
  (interactive)
  (let* ((command "ci")
	 (buf (vc-starteam-actual-buffer-current-activity))
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (files (list (concat dir file))))

    (save-excursion 
      ;; save the buffer containing the file being checked in
      (set-buffer buf)
      (save-buffer)
      
      (message "Checking in file %s%s ..." dir file)

      (pop-to-buffer (get-buffer-create "*StarTeam [Log for Add/Checkin]*"))
      (vc-starteam-log-mode command files buf force))))


(defun vc-starteam-add ()
  "Add the file in the current buffer"
  (interactive)

  (let* ((command "add")
	 (buf (vc-starteam-actual-buffer-current-activity))
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (files (list (concat dir file))))

    ;; save the buffer containing the file being added
    (save-excursion 
      (set-buffer buf)
      (save-buffer)
      
      (message "Adding in file %s%s ..." dir file)

      (pop-to-buffer (get-buffer-create "*Vc-Starteam-Log*"))
      (vc-starteam-log-mode command files buf nil)
      )))



(defun vc-starteam-get-file-history ()
  "Get the status of the file in the current buffer"
  (interactive)
  (let* ((command "hist")
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (output-buffer))
    
    (message "Getting history of %s%s ..." dir file)
    (setq output-buffer (vc-starteam-execute nil command "GET FILE HISTORY" path file 
					  "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
    (vc-starteam-display-buffer output-buffer)
    (message "History placed in buffer <%s>" output-buffer)
    ))

(defun vc-starteam-update-status ()
  "Update the status of the file in the current buffer"
  (interactive)
  (let* ((command "update-status")
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (output-buffer))
    (message "Updating status of %s%s ..." dir file)
    (setq output-buffer (vc-starteam-execute nil
			 command "UPDATE FILE STATUS" path file 
			 "-v" "-contents" 
			 "-rp" (vc-starteam-get-working-dir-from-local-path dir)))
    (message "Status of %s%s is now: %s" 
	     dir file (vc-starteam-get-file-status-as-string))
    ))

(defun vc-starteam-lock-file ()
  "Lock the file in the current buffer"
  (interactive)
  (vc-starteam-perform-lock-operation nil) ; lock
  )

(defun vc-starteam-unlock-file ()
  "Unlock the file in the current buffer"
  (interactive)
  (vc-starteam-perform-lock-operation t) ; unlock
  )

(defun vc-starteam-perform-lock-operation (&optional unlock)
  "Lock the file in the current buffer

unlock:
   if non-nil, will unlock the file instead of locking it"
  (let* ((command "lck")
	 (dir-and-file (vc-starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (lock-operation (if unlock "-u" "-l"))
	 (read-write-operation (if vc-starteam-keep-unlocked-files-read-only 
				   ;; set read-only status as appropriate
				   (if unlock "-ro" "-rw")
				 ;; else don't modify read-only status
				 nil)) 
	 (output-buffer)
	 (operation-success-magic-text (if unlock "unlocked" "locked")))

    (message "%s file %s%s ..." (if unlock "Unlocking" "Locking") dir file)

    (setq output-buffer (vc-starteam-execute nil command "LOCK FILE" path file lock-operation read-write-operation))
    ;(message "FINISHED %s file %s%s ..." (if unlock "Unlocking" "Locking") dir file)

    ;; check operation output buffer for errors
    (message "Checking <%s> for \"%s\"" output-buffer (format "^%s: %s" file operation-success-magic-text))

    (if (not (vc-starteam-output-buffer-contains output-buffer 
					      (format "^%s: %s" file operation-success-magic-text)))

	;; operation failed. Abort with explanation.
	(let ((reason (cond
		       ;; someone else has it locked
		       ((vc-starteam-output-buffer-contains output-buffer "already locked by another user")
			"It is exclusively locked by someone else")
		       ;; file not in repository
		       ((vc-starteam-output-buffer-contains output-buffer (format "No files matching \"%s\"" file))
			"File is not in the Starteam repository")
		       ;; not sure why it failed
		       (t 
			(format "See buffer <%s> for more details." output-buffer)))
		      ))
	  (error "ABORTED. %s was NOT %s. %s."  file operation-success-magic-text reason)
	  )

	  ;; operation was successful
	  (message "%s successfully %s" file operation-success-magic-text)
      )
    
    (revert-buffer t t) ; to refresh read-only status
    (message "%s is now: %s" file operation-success-magic-text)
    ))

(defun vc-starteam-dired-revert (&optional arg noconfirm)
  "Reload the current buffer"
  (interactive)
  (vc-starteam-get-directory default-directory))

(defun vc-starteam-get-directory (indir)
  "Get the status of the directory in the current buffer"
  (interactive "DDirectory name:")
  (let* ((command "list")
	 (path nil)
	 (dir (expand-file-name indir))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir))
	 (still-looking t))
    (message "Checking directory %s ..." dir )
    (switch-to-buffer  
     (vc-starteam-execute nil command "LIST DIRECTORY" path  "*"
		       "-rp" (vc-starteam-get-working-dir-from-local-path dir) ) t)
    (cd dir)
    (vc-starteam-dired-mode)
    ))

(defun vc-starteam-remove-empty-directory-listings ()
  "Get rid of empty Folders"
  (interactive)
  (let ((kill-whole-line t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Folder:.*\nFolder:" nil t)
	(goto-char (match-beginning 0))
	(kill-line))))
  )

(defun vc-starteam-get-directory-of-files-to-checkout (dir)
  (interactive "DDirectory Name:")
  (vc-starteam-get-directory-of-files-filtered dir "IOG"))

(defun vc-starteam-get-directory-of-files-to-checkin (dir)
  (interactive "DDirectory Name:")
  (vc-starteam-get-directory-of-files-filtered dir "MGN"))

(defun vc-starteam-get-directory-of-files-filtered (rawdir filter)
  "Get the status of the directory in the current buffer"
  (interactive "DDirectory name:
sFilter [MCONIGU]*")
  (let* ((command "list")
	 (dir (expand-file-name rawdir))
	 (path (vc-starteam-get-vc-starteam-path-from-local-path dir)))
     (message "Checking directory %s (with filter)..." dir )

     (switch-to-buffer (vc-starteam-execute nil command filter path "-is" "-filter" filter  ) t)
       
     ;;****************************************
     ;;                                        
     ;;  remove the lines of files not needing checkout
     ;;                                        
     ;;****************************************
     (message "Getting rid of all the Current files ... ")
     (save-excursion
       (goto-char(point-min))
       (delete-matching-lines "^Current"))

     (vc-starteam-remove-empty-directory-listings)
     (cd dir)
     (vc-starteam-dired-mode)     
     ))

(defun vc-starteam-ediff ()
  "Get the diff of the file in the current buffer"
  (interactive)
  (let ((buf (cond ((eq major-mode 'vc-starteam-log-mode) vc-starteam-log-file)
		    ((eq major-mode 'vc-starteam-dired-mode)
		     (progn (find-file (dired-get-filename)) (current-buffer)))
		    (t (current-buffer)))))
    
    (save-excursion
      (if vc-starteam-log-file (set-buffer vc-starteam-log-file) nil)
      (let* ((temp-file (vc-starteam-checkout-temp-file)))
	(save-buffer)
	(if temp-file
	    (ediff buffer-file-name temp-file)
	  ;;nil
	  (vc-starteam-error-file-not-in-view buffer-file-name) )))
    ))

(defun vc-starteam-emerge-quit ()
  ""
  (if (and (boundp emerge-prefix-argument) (not emerge-prefix-argument))
      (vc-starteam-checkin-file t) nil )
  )

(defun vc-starteam-merge-current-buffer ()
  "Merge the current buffer with the current version"
  (interactive)
  (let* ((temp-file (vc-starteam-checkout-temp-file)))
    
    (save-buffer)
    (add-hook 'quit-hook 'vc-starteam-emerge-quit)
    (if temp-file
	(emerge-files t buffer-file-name temp-file buffer-file-name nil quit-hook)
	;;(ediff-files buffer-file-name temp-file)
      ;;nil
    (vc-starteam-error-file-not-in-view buffer-file-name)  )))

(defun vc-starteam-next-action ()
  "Performs the next appropriate action on the file in the current
buffer.

Current Status          Action Performed
--------------          ----------------
Out of Date             vc-starteam-checkout-file
Missing                 vc-starteam-checkout-file
Current                 (message \"Up to date\")
Modified                vc-starteam-checkin-file
Merge                   vc-starteam-merge
Unknown                 vc-starteam-update-status
Not in View             vc-starteam-add"
  (interactive)
  (let ((status (vc-starteam-get-file-status-as-string)))
    (cond ((equal status "Out of Date")
	   (vc-starteam-checkout-file))
	  ((equal status "Missing")
	   (vc-starteam-checkout-file))
	  ((equal status "Current")
	   (message "%s" "Up to date"))
	  ((equal status "Modified")
	   (vc-starteam-checkin-file))
	  ((equal status "Merge")
	   (vc-starteam-merge))
	  ((equal status "Unknown")
	   (vc-starteam-update-status))
	  ((equal status "Not in View")
	   (vc-starteam-add))
	  (t
	   (error "Cannot perform next operation because status is %s" status))
	  )))


(defun vc-starteam-execute (buffer command operation-string path &optional file &rest args)
  "Execute a StarTeam command. 

Returns the name of the buffer into which the output of the 
command was written.

Arguments:
----------

buffer
     the buffer to write the contents to.  If nil, use 
     *Starteam [ <command> ] <path>*
command
     command to send to vc-starteam-executable 
     (e.g., \"co\", \"hist\", \"list\", etc

operation-string
     human-readable description of the operation 
     (e.g., \"check out\", \"file history\", \"file status\", etc)

path
     starteam path to file's directory, in the form PROJECT/VIEW/<...path...>
     (e.g. \"MyProject/MyView/src/win32/core\")

file 
     file to operate upon

args 
     additional arguments to pass to vc-starteam-executable
"
  (if vc-starteam-debug
      (message "Starteam %s operation: [ %s %s %s ]" 
	       operation-string
	       (concat vc-starteam-executable " " command " -x -p \"" 
		       (vc-starteam-get-login-info) "/" path "\"" )
	       (vc-starteam-stringlist-to-single-string args) file)
    nil)
  (let ((bname (if buffer buffer
		 (concat 
		  "*Starteam [" command "] " 
		  path 
		  "*"))))

    (save-excursion
      (get-buffer-create bname)
      (set-buffer bname)
      (toggle-read-only -1)
      (erase-buffer)
      )

    ;(message "starteam executing [%s %s %s]" (concat vc-starteam-executable " " command " -x -p \"" (vc-starteam-get-login-info) "/" path "\"" ) (vc-starteam-stringlist-to-single-string args) file)
    (if args 
	;(apply 'call-process "C:\\winnt\\system32\\cmd.exe" nil bname  nil "/C" "echo" "ECHOING: "
	(apply 'call-process vc-starteam-executable nil bname  nil
	       command
	       "-nologo" ; don't print executable version
	       "-x"      ; non-interactive
	       "-p"
	       (concat (vc-starteam-get-login-info) "/" path)
	       (append (vc-starteam-remove-bad-vc-starteam-args args) (list file))
	       )
      ;(apply 'call-process "C:\\winnt\\system32\\cmd.exe" nil bname nil "/C" "echo" "BOO"
      (apply 'call-process vc-starteam-executable nil bname nil
	     command
	     "-nologo"
	     "-x"
	     "-p"
	     (concat (vc-starteam-get-login-info) "/" path)
	     (if file (list file) nil)
	     )
      )

    (save-excursion
      (set-buffer bname)
      (goto-char (point-min))
      )

  bname))

(define-derived-mode vc-starteam-dired-mode dired-mode vc-starteam-dired-mode-name
  "The major mode used in VC directory buffers.  It works like Dired,
but lists only files under version control, with the current VC state of 
each file being indicated in the place of the file's link count, owner, 
group and size.  Subdirectories are also listed, and you may insert them 
into the buffer as desired, like in Dired.
  All Dired commands operate normally, with the exception of `v', which
is redefined as the version control prefix, so that you can type 
`vl', `v=' etc. to invoke `vc-print-log', `vc-diff', and the like on
the file named in the current Dired buffer line.  `vv' invokes
`vc-next-action' on this file, or on all files currently marked.
There is a special command, `*l', to mark all files currently locked."
  ;;(make-local-hook 'dired-after-readin-hook)
  ;;(add-hook 'dired-after-readin-hook 'vc-starteam-dired-hook nil t)
;;  (make-local-hook 'dired-mode-hook)
 ;; (add-hook 'dired-mode-hook 'vc-starteam-dired-hook nil t)
  ;; The following is slightly modified from dired.el,
  ;; because file lines look a bit different in vc-dired-mode.
  (set (make-local-variable 'dired-move-to-filename-regexp)
       "\\(\\(.ot in View\\|.ut of Date\\|.urrent\\|.odified\\|.issing\\|.p to Date\\|.erge\\|.nknown\\)\\([ A-Za-z\t]*r*[w\-]*  *[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+[A-Z ]+[0-9]+ \\)\\|\\([^:]*:[0-9][0-9]\\s-*\\)\\)\\|\\(Folder:.*(working dir: \\)")
  (set (make-local-variable 'dired-permission-flags-regexp)
       " [ r][ w] ")
  (set (make-local-variable 'dired-subdir-alist)
       (vc-starteam-dir-get-subdirs))
  (and (boundp 'vc-starteam-dired-switches)
       vc-starteam-dired-switches
       (set (make-local-variable 'dired-actual-switches)
            vc-starteam-dired-switches))
;;  (set (make-local-variable 'vc-starteam-dired-terse-mode) vc-starteam-dired-terse-display)
  (setq vc-dired-mode t))

(defun vc-starteam-dir-get-subdirs ()
  "Get the list of sub directories"
  (interactive)
  (save-excursion
    (let ((rtnval nil))
      (goto-char (point-min))
      (while (re-search-forward "working dir: \\([^)]*\\)" nil t)
	(if rtnval
	    (setq rtnval
		  (cons
		   (cons
		    (concat (buffer-substring-no-properties (match-beginning 1) (match-end 1)) "/")
		    (match-beginning 1)) rtnval))
	  (setq rtnval (list (cons
			(concat (buffer-substring-no-properties (match-beginning 1) (match-end 1)) "/")
			(match-beginning 1))))))
      rtnval)))
      
(defun vc-starteam-log-mode (&optional command files buf force)
  "Minor mode for driving version-control tools.
These bindings are added to the global keymap when you enter this mode:
\\[vc-starteam-get-file-history]		display change history of current file
\\[vc-starteam-ediff]		show diffs between file versions

While you are entering a change log message for a version, the following
additional bindings will be in effect.

\\[vc-starteam-finish-logentry]	proceed with check in, ending log message entry

"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map vc-starteam-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'vc-starteam-log-mode)
  (setq mode-name "Vc-Starteam-Log")

  (make-local-variable 'vc-starteam-checkin-file)
  (make-local-variable 'vc-starteam-force-checkin)
  (make-local-variable 'vc-starteam-last-buffer)
  (make-local-variable 'vc-starteam-command)

  (setq vc-starteam-force-checkin force)
  (setq vc-starteam-checkin-files files)
  (setq vc-starteam-last-buffer buf)
  (setq vc-starteam-command command)
  
  (make-local-variable 'vc-starteam-log-version)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'vc-log-mode-hook)
  )


;; ------------------------------------------------------------------
;;                   UTILITY ROUTINES
;; ------------------------------------------------------------------


;; Takes a list of strings and concatenates them with a space between each element
(defun vc-starteam-stringlist-to-single-string (args)
  (let ((firstElement (car args))
	(remainingElements (cdr args)))
    (concat firstElement 
	    (if remainingElements 
		(concat " " (vc-starteam-stringlist-to-single-string remainingElements)) 
	      nil))))

(defun vc-starteam-error-file-not-in-view (path)
  (error "Could not find %s in any known Starteam view" path)
)

;; this is necessary because starteam gets confused if it gets blank
;; arguments sent to it via 'apply.
(defun vc-starteam-remove-bad-vc-starteam-args (arglist)
  "Removes the elements in the argument that begin with a space, are
are nil, or are the empty string. Returns the filtered list."  
  (let
      ((non-nil-args ()))
    (while arglist
      (let ((element (car arglist)))
	;; only include the element if it is not nil, it is not the empty
	;; string, and its first character is not a space
	;;
	;; -- TO DO: should probably make sure the element is a string
	;;    before doing string ops on it
	(if (and element 
		 (not (equal element "")) 
		 (not (equal (substring element 0 1) " ")) )
	    (setq non-nil-args (append non-nil-args (list element)))
	  nil)
	)
      (setq arglist(cdr arglist))
      )
    non-nil-args
    )
)

(defun vc-starteam-actual-buffer-current-activity ()
  "Examines the current buffer and, based on the buffer mode, returns
the buffer containing the file. 

Handles special cases such as vc-starteam-log-mode and
vc-starteam-dired-mode. For example, when the point is over a file in
vc-starteam-dired-mode, this method opens the file and returns the buffer
for the file underneath the point instead of returning the dired
buffer."
  (interactive)
  (let* ((buf (cond 
	       
	       ;; vc-starteam-log-file
	       ((eq major-mode 'vc-starteam-log-mode) vc-starteam-log-file)
		    
	       ;; vc-starteam-dired-mode 
	       ;;
	       ;; Note: We have to open the file under the point, get
	       ;; the buffer, then switch back to the dired
	       ;; buffer. Otherwise the buffer in view after the
	       ;; function call would be different than the one in
	       ;; view before the call.
	       ((eq major-mode 'vc-starteam-dired-mode) 
		(let ((buffer-file-under-point))
		  (save-excursion 
		    (find-file (dired-get-filename)) 
		    (setq buffer-file-under-point (current-buffer))
		    )
		  (switch-to-buffer (current-buffer))
		  buffer-file-under-point))
	       
	       ;; any other mode
	       (t (current-buffer)))))
    buf))

(defun vc-starteam-current-buffer-dir-and-file ()
  "Examines the current buffer and returns the appropriate file directory name 
based on the buffer mode. Values are returned as a list in the form 

     (directory file)

Calls 'error if the current buffer is not associated with a file."
  (interactive)
  (let* ((buf (vc-starteam-actual-buffer-current-activity))
	 (file (file-name-nondirectory (buffer-file-name buf)))
	 (bfilename (buffer-file-name buf))
	 (dir))
    
    (if bfilename
	(setq dir (file-name-directory bfilename))
      (error "Buffer \"%s\" not associated with a file" (current-buffer)))
    (list dir file)
    ))

;; NOTE: this method is not very conservative. For example, assume
;; vc-starteam-to-directory-alist looks like this:
;;
;;  (setq vc-starteam-to-directory-alist (list (cons  "^X:/test"  "test/chrisview" )))
;;
;; then (vc-starteam-get-vc-starteam-path-from-local-path "X:/test/a/b") --> "test/chrisview/a/b"
;; which is correct. 
;; 
;; Unfortunately, (vc-starteam-get-vc-starteam-path-from-local-path "X:/test27/a/b") --> "test/chrisview27/a/b"
;; which is incorrect; instead it should have signalled an error
;;
(defun vc-starteam-get-vc-starteam-path-from-local-path (local-dir &optional supress-error)
  "Given a local directory, attempts to convert it to a starteam path
based upon the values in vc-starteam-to-directory-alist. If the given directory
cannot be reconciled to a starteam path, 'error is called.

For example, assume that vc-starteam-to-directory-alist is set up as follows:

 (setq vc-starteam-to-directory-alist (list (cons  \"^X:/test\"  \"test/myview\" )))

In this case

 (vc-starteam-get-vc-starteam-path-from-local-path \"X:/test/a/b\") 

returns \"test/myview/a/b\"
"
  (let ((still-looking t)
	(path nil))
    (mapcar (function (lambda (x)
			(if (and still-looking
				 (string-match (car x) local-dir))
			    (setq still-looking nil
				  path (concat (cdr x)
					       (substring local-dir (match-end 0))))
			  nil)))
	    vc-starteam-to-directory-alist)
    (if (not path)
		(if supress-error nil
		  (error "Could match local path %s to path in any known Starteam view" local-dir))
      path)
    ))

(defun vc-starteam-get-working-dir-from-local-path (local-dir)
  "Given a local directory, attempts to determine the starteam working directory
based upon the values in vc-starteam-to-directory-alist. If the given directory
cannot be reconciled to a starteam path, 'error is called.

For example, assume that vc-starteam-to-directory-alist is set up as follows:

 (setq vc-starteam-to-directory-alist (list (cons  \"^X:/test\"  \"test/myview\" )))

In this case, both

 (vc-starteam-get-working-dir-from-local-path \"X:/test/a/b/foo.java\") 

and

 (vc-starteam-get-working-dir-from-local-path \"X:/test/g/bar.c++\") 

return \"X:/test\"
"
  (let* ((working-dir nil)
	 (still-looking t))
    (mapcar  (lambda (x)
	       (if (and still-looking
			(string-match (car x) local-dir))
		   (setq still-looking nil
			 working-dir (substring local-dir 
					   (match-beginning 0) 
					   (match-end 0)))
		 nil))
	    vc-starteam-to-directory-alist)
    (if (not working-dir)
	(error "Could match local path %s to path in any known Starteam view" local-dir)
      working-dir)
    ))

(defun vc-starteam-output-buffer-contains (buffer regexp)
  "Searches the given buffer for the occurence of the given regular 
expression; returns t if the regexp was found, and nil otherwise."
  (if vc-starteam-debug (message "Searching buffer %s for regexp %s" buffer regexp))
  (save-excursion 
    (set-buffer buffer)
    (goto-char (point-min))      
    (if (re-search-forward regexp nil t) 
	t 
      nil) 
    ))

(defun vc-starteam-display-buffer (buffer)
  "Displays the given buffer according to the value of vc-starteam-switch-to-output"
  (cond ((equal vc-starteam-switch-to-output t)   (switch-to-buffer buffer))
	((equal vc-starteam-switch-to-output nil) nil)
	(t (shrink-window-if-larger-than-buffer (display-buffer buffer t)))
	))

(defun vc-starteam-print-message-file-status ()
  "Checks the status of the file in the current buffer and 
prints it to the mini-buffer"
  (interactive)
  (message "Status of %s is: %s" (buffer-file-name (vc-starteam-actual-buffer-current-activity)) (vc-starteam-get-file-status-as-string)))

(defun vc-starteam-menubar-setup ()
  "Adds a menu to the menu bar containing vc-starteam-mode operations"
  (interactive)
  
  ;;;(define-key-after menu-bar-tools-menu [starteam] (cons "STARTEAM" vc-starteam-menu-map) 'vc) 
  (define-key global-map [menu-bar starteam] (cons "STARTEAM" vc-starteam-menu-map))

  (define-key vc-starteam-menu-map [vc-starteam-dired-with-filters] '("List Directory w/ Filters" . vc-starteam-get-directory-of-files-filtered))
  (define-key vc-starteam-menu-map [vc-starteam-dired]      '("List Directory" . vc-starteam-get-directory))
  (define-key vc-starteam-menu-map [separator4]          '("--"))
  (define-key vc-starteam-menu-map [vc-starteam-update-status] '("Update status" . vc-starteam-update-status))
  ;; disabled merge until merge bug is fixed
  ;;(define-key vc-starteam-menu-map [vc-starteam-merge]      '("Merge" . vc-starteam-merge))
  (define-key vc-starteam-menu-map [vc-starteam-add]        '("Add" . vc-starteam-add))
  (define-key vc-starteam-menu-map [separator3]          '("--"))
  (define-key vc-starteam-menu-map [vc-starteam-status]     '("Show Status" . vc-starteam-print-message-file-status))
  (define-key vc-starteam-menu-map [vc-starteam-history]    '("Show History" . vc-starteam-get-file-history))
  (define-key vc-starteam-menu-map [vc-starteam-ediff]       '("Show Differences" . vc-starteam-ediff))
  (define-key vc-starteam-menu-map [separator2]          '("--"))
  (define-key vc-starteam-menu-map [vc-starteam-checkout]   '("Check Out" . vc-starteam-checkout-file))
  (define-key vc-starteam-menu-map [vc-starteam-checkin]    '("Check In" . vc-starteam-checkin-file))
  (define-key vc-starteam-menu-map [separator1]          '("--"))
  (define-key vc-starteam-menu-map [vc-starteam-unlock]     '("Unlock" . vc-starteam-unlock-file))
  (define-key vc-starteam-menu-map [vc-starteam-lock]       '("Lock (exclusive)" . vc-starteam-lock-file))
  )


;; (define-key vc-starteam-dired-mode-map "\C-xv" vc-prefix-map)
;; (define-key vc-starteam-dired-mode-map "v" vc-prefix-map)
;; (define-key vc-starteam-dired-mode-map "g" 'vc-starteam-dired-revert)

;; (global-set-key "v=" (quote vc-starteam-ediff))
;; (global-set-key "vm" (quote vc-starteam-merge))
;; (global-set-key "vv" (quote vc-starteam-next-action))
;; (global-set-key "vl" (quote vc-starteam-get-file-history))
;; (global-set-key "v\C-l" (quote vc-starteam-lock-file))
;; (global-set-key "v\C-u" (quote vc-starteam-unlock-file))
;; (global-set-key "vd" (quote vc-starteam-get-directory))
;; (global-set-key "v" (quote vc-starteam-get-directory-of-files-to-checkout))
;; (global-set-key "v	" (quote vc-starteam-get-directory-of-files-to-checkin))


(defvar vc-starteam-log-entry-mode nil)
(defvar vc-starteam-log-file nil)
(defvar vc-starteam-log-version nil)

;; Initialization code, to be done just once at load-time
(if vc-starteam-log-entry-mode
    nil
  (setq vc-starteam-log-entry-mode (make-sparse-keymap))
  (define-key vc-starteam-log-entry-mode "\C-c\C-c" 'vc-starteam-finish-logentry)
  
  )

(add-to-list 'vc-handled-backends 'STARTEAM t) ;Append STARTEAM as a vc backend

;(add-hook 'ediff-cleanup-hook '(lambda nil
;				 (bury-buffer ediff-buffer-B)))


(provide 'vc-starteam)
