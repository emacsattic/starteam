;;; starteam.el --- VC-type mode for StarTeam

;;; Copyright (C) 2002 Matthew O. Smith
;;; Copyright (C) 2001 Nearlife, Inc.
;;; Copyright (C) 1999 Matthew O. Smith

;; Filename: starteam.el
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
;; Contributors:
;;   Matthew O. Smith
;;   Christopher J. Kline
;;   Stephan Zitz <szitz at globalscape dot com>
;;      Added support for mapping cygwin drives under Win32 and XEmacs
;;
;;
;;   This file implements many of the features of VC for use with
;;   StarTeam (www.starbase.com), a source control program.
;;
;;
;;   ^X-v-v    - Do next action:
;;	Current ->     Display the message "Up to Date"
;;	Out of Date -> Do a check out
;;	Merge ->       Run emerge
;;	Modified ->    Checkin with a comment
;;   ^X-v-l    - Get a history of the file
;;   ^X-v-=    - Do an ediff on the contents of the current buffer with
;;               the latest version in source control
;;   ^X-v-d    - Get a status of all files in the current directory
;;   ^X-v-^I   - Get a list of files to check in in for a directory and all sub directories
;;   ^X-v-^O   - Get a list of files to check out in for a directory and all sub directories
;;   ^X-v-^L   - Lock the file
;;   ^X-v-^U   - Unlock the file
;;
;; Configuration:
;;
;; At a minimum you need to set the following:
;;
;;   (setq starteam-host "10.42.42.2"   ; or "myMachine.myCompany.com"
;;         starteam-executable "stcmd"
;;         starteam-to-directory-alist (list (cons  "^/export/home/username/working"  "src/src" )))
;;
;; You will need to customize starteam-to-directory-alist to your
;; particular Starteam projects; see the doc comments for more
;; details on the structure of this variable.
;;
;; In addition, there are also several other variables that configure
;; the behavior of starteam.el:
;;
;;    starteam-dired-mode-name
;;    starteam-port
;;    starteam-user
;;    starteam-password
;;    starteam-keep-unlocked-files-read-only
;;    starteam-unlock-after-checkin
;;    starteam-switch-to-output
;;    starteam-debug
;;    starteam-post-checkin-functions
;;
;; Users may also wish to call 
;;  
;;    starteam-menubar-setup 
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
;;             getting file history) is now configurable via starteam-switch-to-output
;;           - Added starteam-unlock-file
;;           - Added starteam-menubar-setup to add STARTEAM menu to main menubar
;;           - Check for failure of most operations, output descriptive error messages
;;           - If check-out fails, reason is given and option to force check-out is 
;;             presented
;;           - Most operations now fail gracefully on common failure conditions 
;;             (usually when buffer file cannot be reconciled with 
;;             starteam-to-directory-alist)
;;           - Functions now restore current-buffer to original state before exiting
;;             (except starteam-actual-buffer-current-activity; see "Known Bugs")
;;           - Consolidated redundant code
;;
;; 0.4    [Christopher J. Kline] 09 May 2001         
;;        - starteam-perform-lock-operation now checks to make sure
;;          that the operation succeeded; if it does not, will try to
;;          provide an explanation of why the operation failed
;;        - changed (file (buffer-name buf)) to 
;;          (file (file-name-nondirectory (buffer-file-name buf))) in
;;          starteam-current-buffer-dir-and-file so that file name
;;          returned is based on the file associated with the buffer
;;          instead of the buffer name (fixed previous bug which
;;          prevented operations from working when the 'uniquify'
;;          package was operating)
;;
;; 0.5    [Christopher J. Kline] 21 June 2001         
;;        - fixed bug in starteam-dired-mode where "Unknown" status
;;          or a "-" in the file mode would cause a "no file this line" 
;;          error 
;;
;; 0.6    [Matthew O. Smith] 08 May 2002
;;        - added starteam-checkout-dir-recursive
;;        - Chris Kline no longer has access to Starteam, and Matthew Smith has
;;          resumed using it, so Matthew is taking over maintenance of starteam.el

;; To do:
;;    - Have Merge do an optional checkin
;;    - Have get-directory enter a dired type mode
;;    - Integrate fully into vc-mode menus
;;
;; Known bugs:
;;    - Merge checks in the wrong file (disabled merge menu option
;;      until this is tested and fixed)
;;    - When in dired mode, some operations (getting status, locking,
;;      unlocking) switch the current buffer to the
;;      output-buffer. This should not happen, and is a side-effect of
;;      starteam-actual-buffer-current-activity's reliance on
;;      'find-file when in starteam-dired-mode

(eval-when-compile
  (progn
    (require 'dired)
    (require 'string)
    (require 'vc)))
(require 'dired)
(require 'string)
(require 'vc)

(defvar starteam-map-cygdrive nil "Should /cygdrive/_DRIVE_/ be mapped to _DRIVE_:")

(defvar starteam-dired-mode-name "Dired under StarTeam" "The name of the dired mode")

(defvar starteam-host "sisyphus" "*Host for  StarTeam")

(defvar starteam-port 49201 "*Port on starteam-host to talk to")

(defvar starteam-user nil "*Who to log onto StarTeam as")

(defvar starteam-password nil "password to log onto StarTeam with")

;; NOTE: if you are actually setting starteam-executable on win32, the
;; value should have only 4 backslashes per backslash in the
;; path. There are 8 in the doc comments only so it shows correctly
;; when viewed via (describe-variable 'starteam-executable)
(defvar starteam-executable "stcmd.exe" "The name of the Starteam command-line client. 
This might be \"stcmd\" if the executable is in your path; \"/usr/local/bin/stcmd\" 
if you are on a unix platform; or \"C:\\\\\\\\Program Files\\\\\\\\StarTeam 4.0\\\\\\\\stcmd.exe\"
if you are on win32.")

(defvar starteam-keep-unlocked-files-read-only nil "If non-nil, files that are unlocked will be marked read-only. Mimics the \"Mark unlocked working files read-only\" option in the Win32 Starteam client's \"Workstation Options\" dialog")

(defvar starteam-unlock-after-checkin nil "If non-nil, files will be unlocked after they are checked in.")

(defvar starteam-switch-to-output t 
  "Determines how the output of a starteam query command (such as starteam-get-file-history) 
is made visible. If is t, the current buffer will be switched to the
output buffer. If nil then current buffer will remain current the
output buffer will NOT be displayed. If not t or nil then the current
buffer will remain current and the output buffer will be displayed via
'display-buffer.")

;; For debugging
(defvar starteam-debug nil "If non-nil, outputs extra debugging info")

(defvar starteam-to-directory-alist
  '(
    ( "^C:/starteam/view_folder" . "project/view" )
    )
  "*Map the directory to the starteam project.
The elements in the starteam-to-directory-alist are of the form:

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
")

(defvar starteam-post-checkin-functions nil
  "A list of functions to call after a successful checkin.
Each function is called with the arguments FILES and REASON.")


(defvar starteam-menu-map (make-sparse-keymap "STARTEAM"))

(defun starteam-get-user ()
  "Get the name of the user for StarTeam"
  (interactive)
  (if starteam-user starteam-user
    (progn
      (setq starteam-user (read-string "Enter user name: " (user-login-name)))
      starteam-user)))
 

(defun starteam-get-password ()
  "Get the password for the user of StarTeam"
  (interactive)
  (if starteam-password starteam-password
      (setq starteam-password (read-passwd (concat "Enter password for user "
					       (starteam-get-user)
					       ": ")))))

(defun starteam-get-login-info ()
  "Get the login information for the starteam server.

Returns a string in the form:

      USER:PASSWORD@HOST_MACHINE:PORT"
  (concat (starteam-get-user) ":"
	  (starteam-get-password) "@"
	  starteam-host ":" starteam-port))


(defun starteam-check-file-status ()
  "Checks the status of the file in the current buffer
and returns the buffer containing the output of the status check"
  (interactive)
  (let* ((command "list")
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (output-buffer))

    (message "Checking status of %s%s ..." dir file)

    (save-excursion
      (setq output-buffer 
	    (starteam-execute command 
			      "GET FILE STATUS" path file
			      "-rp" (starteam-get-working-dir-from-local-path dir)))
      )
    output-buffer 
    ))

(defun starteam-get-file-status-as-string ()
  "Calls starteam-check-file-status, then searches the status buffer 
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
	(status-buffer (starteam-check-file-status)))

    (save-excursion
      (set-buffer status-buffer)
      (goto-char (point-min))      
      (if (re-search-forward "^\\(Out of Date\\|Unknown\\|Current\\|\\Modified\\|\\Missing\\|Merge\\|Not in View\\)" nil t)
	  ; return the matched string
	  (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	(error "Error checking status; see buffer %s" status-buffer))
      )
    ))

(defun starteam-checkout-file (&optional force)
  "Checkout the file in the current buffer

force - if non-nil, forces the checkout"
  (interactive)
  (let* ((command "co")
	 (buf (starteam-actual-buffer-current-activity))
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (file (cadr dir-and-file))	 
	 ;; if this file is missing from the working directory and
	 ;; starteam-keep-unlocked-files-read-only is non-nil, then
	 ;; we'll explicity tell the starteam command to set the newly
	 ;; checked-out file to be read-only (because by default the
	 ;; starteam command-line client sets newly checked-out files
	 ;; to read-write). We also have to unlock the file since
	 ;; starteam doesn't allow setting read-only outside of the
	 ;; context of a lock or unlock operation. Jeesh!
	 (must-make-file-read-only (if (and starteam-keep-unlocked-files-read-only 
					    (starteam-is-file-locked-by-user))
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
      (setq output-buffer (starteam-execute command "CHECK OUT FILE" path file 
					    read-write-operation unlock-operation 
					    "-rp"
					    (starteam-get-working-dir-from-local-path dir)
					    (if force "-o" nil) ; force checkout
					    ))
      (if (not (starteam-output-buffer-contains output-buffer (format "^%s: checked out" file)))

	  ;; local file is different from starteam file; confirm the checkout
	  (let ((file-status (starteam-get-file-status-as-string)))
	    (if (yes-or-no-p (format "Status of %s%s is: %s. Force checkout of file?" 
				     dir file file-status))
		;; do a forced checkout
		(starteam-checkout-file t) 

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

(defun starteam-checkout-dir-recursive () 
  "Checkout the current directory and all subdirectories"               
  (interactive)                                        
  (let* ((command "co")               
         (buf (current-buffer)) 
         (dir (expand-file-name default-directory)) 
         (path (starteam-get-starteam-path-from-local-path 
		dir)) 
         (output-buffer) 
         ) 
         
    (save-excursion  
      (set-buffer buf)                       
      (save-buffer)) 
                                                       
    (message "Checking out dir %s ..." dir ) 
        
    (save-excursion 
      (setq output-buffer (starteam-execute command 
					    "CHECK OUT FILE" path  
                                            "-is" 
                                            "-rp" 
                                            (starteam-get-working-dir-from-local-path 
					     dir) 
                                            )) 
       
      (message "Checked out dir %s ..." dir) 
      ))) 

(defun starteam-checkout-temp-file ()
  "Checkout the file in the current buffer into a temp dir.  Return the path name of the temp file"
  (interactive)
  (let* ((command "co")
	(temp-directory (concat temporary-file-directory "/StarTeamTemp-" user-full-name))
	(dir (file-name-directory (buffer-file-name)))
	(file (buffer-name))
	(path (starteam-get-starteam-path-from-local-path dir)))

    (save-buffer)
    (message "Checking out temporary version of file %s%s to %s/%s%s" 
	     dir file temp-directory dir file)
    (save-excursion
      (set-buffer (starteam-execute command "TEMPORARY CHECK OUT OF FILE" path file 
				    "-rp" temp-directory))
      (goto-char (point-min))
      (if (re-search-forward "(working dir: \\(.*\\))" nil t)
	  (concat (buffer-substring-no-properties (match-beginning 1)
						  (match-end 1))
		  "/" file)
	nil)
      )
    ))

;; maybe should have starteam-check-file-status return vector of all
;; status elements? Then this method could simply check to see if the
;; file is locked by the current user.
(defun starteam-is-file-locked-by-user ()
  (interactive)
  
  (string-equal (starteam-check-file-status)  "Missing")
)

(defun starteam-finish-logentry ()
  "Do the actual checking in or adding of files once they have had their log entry set."
  (interactive)
  (let* ((reason (buffer-string))
	 (force (if (boundp 'starteam-force-checkin) starteam-force-checkin nil))
	 (buf starteam-last-buffer)
	 (files starteam-checkin-files)
	 (command starteam-command)
	 (unlock-operation (if starteam-unlock-after-checkin "-u" nil)) 
	 (read-write-operation-for-checkin (if (and starteam-unlock-after-checkin starteam-keep-unlocked-files-read-only)
					       "-ro" 
					     nil)) 
	 (read-write-operation-for-add (if starteam-keep-unlocked-files-read-only "-ro" nil)) 
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
	      (setq path (starteam-get-starteam-path-from-local-path (file-name-directory fullpath)))
	      (setq file (file-name-nondirectory fullpath))
	      (let ((operation-success-magic-text (cond ((string-equal command "ci") "checked in")
							((string-equal command "add") "added")
							(t (error "ERROR: unknown command \"%s\" sent to starteam-finish-logentry" command))
							))
		    (output-buffer))
		(if path
		    (progn
		      (save-excursion
			(cond (
			       ;; CHECKING IN an existing file
			       (string-equal command "ci")			     
			       (setq output-buffer 
				     (starteam-execute command command path file 
						       "-r" reason
						       "-rp" 
						       (starteam-get-working-dir-from-local-path (file-name-directory fullpath))
						       (if force "-o" nil)
						       unlock-operation 
						       read-write-operation-for-checkin
						       ))				 
			       )

			      ;; ADDING a new file
			      ((string-equal command "add")
			       (message "Adding file %s%s to Starteam path %s" (file-name-directory fullpath) file path)			     
			       (setq output-buffer (starteam-execute command command path file 
								     "-rp" (starteam-get-working-dir-from-local-path (file-name-directory fullpath)) 
								     "-u" read-write-operation-for-add "-d" reason))			       
			       )

			      ;; UNKNOWN command
			      ;; (already trapped this error in definition of operation-success-magic-text

			      ) ; end 'cond
			
			;; check operation output buffer for errors
			(message "Checking <%s> for \"%s\"" output-buffer (format "^%s: %s" file operation-success-magic-text))
			(if (not (starteam-output-buffer-contains output-buffer (format "^%s: %s" file operation-success-magic-text)))			    
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
	    starteam-post-checkin-functions)
    (switch-to-buffer buf)
    (revert-buffer nil t)	  
    (delete-window)
    ))

;****************************************
;                                        
;  Check in multiple files
;                                        
;****************************************
(defun starteam-checkin-multiple-files (files &optional force)
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
    (starteam-log-mode command f (current-buffer) force)
    ))

(defun starteam-checkin-file (&optional force)
  "Checkin the file in the current buffer

force:
   if non-nil, forces the checkin "
  (interactive)
  (let* ((command "ci")
	 (buf (starteam-actual-buffer-current-activity))
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (files (list (concat dir file))))

    (save-excursion 
      ;; save the buffer containing the file being checked in
      (set-buffer buf)
      (save-buffer)
      
      (message "Checking in file %s%s ..." dir file)

      (pop-to-buffer (get-buffer-create "*StarTeam [Log for Add/Checkin]*"))
      (starteam-log-mode command files buf force))))


(defun starteam-add ()
  "Add the file in the current buffer"
  (interactive)

  (let* ((command "add")
	 (buf (starteam-actual-buffer-current-activity))
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (files (list (concat dir file))))

    ;; save the buffer containing the file being added
    (save-excursion 
      (set-buffer buf)
      (save-buffer)
      
      (message "Adding in file %s%s ..." dir file)

      (pop-to-buffer (get-buffer-create "*StarTeam-Log*"))
      (starteam-log-mode command files buf nil)
      )))

(defun starteam-get-file-history ()
  "Get the status of the file in the current buffer"
  (interactive)
  (let* ((command "hist")
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (output-buffer))
    
    (message "Getting history of %s%s ..." dir file)
    (setq output-buffer (starteam-execute command "GET FILE HISTORY" path file 
					  "-rp" (starteam-get-working-dir-from-local-path dir)))
    (starteam-display-buffer output-buffer)
    (message "History placed in buffer <%s>" output-buffer)
    ))

(defun starteam-update-status ()
  "Update the status of the file in the current buffer"
  (interactive)
  (let* ((command "update-status")
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (output-buffer))
    (message "Updating status of %s%s ..." dir file)
    (setq output-buffer (starteam-execute
			 command "UPDATE FILE STATUS" path file 
			 "-v" "-contents" 
			 "-rp" (starteam-get-working-dir-from-local-path dir)))
    (message "Status of %s%s is now: %s" 
	     dir file (starteam-get-file-status-as-string))
    ))

(defun starteam-lock-file ()
  "Lock the file in the current buffer"
  (interactive)
  (starteam-perform-lock-operation nil) ; lock
  )

(defun starteam-unlock-file ()
  "Unlock the file in the current buffer"
  (interactive)
  (starteam-perform-lock-operation t) ; unlock
  )

(defun starteam-perform-lock-operation (&optional unlock)
  "Lock the file in the current buffer

unlock:
   if non-nil, will unlock the file instead of locking it"
  (let* ((command "lck")
	 (dir-and-file (starteam-current-buffer-dir-and-file))
	 (dir (car dir-and-file))
	 (file (cadr dir-and-file))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (lock-operation (if unlock "-u" "-l"))
	 (read-write-operation (if starteam-keep-unlocked-files-read-only 
				   ;; set read-only status as appropriate
				   (if unlock "-ro" "-rw")
				 ;; else don't modify read-only status
				 nil)) 
	 (output-buffer)
	 (operation-success-magic-text (if unlock "unlocked" "locked")))

    (message "%s file %s%s ..." (if unlock "Unlocking" "Locking") dir file)

    (setq output-buffer (starteam-execute command "LOCK FILE" path file lock-operation read-write-operation))
    ;(message "FINISHED %s file %s%s ..." (if unlock "Unlocking" "Locking") dir file)

    ;; check operation output buffer for errors
    (message "Checking <%s> for \"%s\"" output-buffer (format "^%s: %s" file operation-success-magic-text))

    (if (not (starteam-output-buffer-contains output-buffer 
					      (format "^%s: %s" file operation-success-magic-text)))

	;; operation failed. Abort with explanation.
	(let ((reason (cond
		       ;; someone else has it locked
		       ((starteam-output-buffer-contains output-buffer "already locked by another user")
			"It is exclusively locked by someone else")
		       ;; file not in repository
		       ((starteam-output-buffer-contains output-buffer (format "No files matching \"%s\"" file))
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

(defun starteam-dired-revert (&optional arg noconfirm)
  "Reload the current buffer"
  (interactive)
  (starteam-get-directory default-directory))

(defun starteam-get-directory (indir)
  "Get the status of the directory in the current buffer"
  (interactive "DDirectory name:")
  (let* ((command "list")
	 (path nil)
	 (dir (expand-file-name indir))
	 (path (starteam-get-starteam-path-from-local-path dir))
	 (still-looking t))
    (message "Checking directory %s ..." dir )
    (switch-to-buffer  
     (starteam-execute command "LIST DIRECTORY" path  "*"
		       "-rp" (starteam-get-working-dir-from-local-path dir) ) t)
    (cd dir)
    (starteam-dired-mode)
    ))

(defun starteam-remove-empty-directory-listings ()
  "Get rid of empty Folders"
  (interactive)
  (let ((kill-whole-line t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Folder:.*\nFolder:" nil t)
	(goto-char (match-beginning 0))
	(kill-line))))
  )

(defun starteam-get-directory-of-files-to-checkout (dir)
  (interactive "DDirectory Name:")
  (starteam-get-directory-of-files-filtered dir "IOG"))

(defun starteam-get-directory-of-files-to-checkin (dir)
  (interactive "DDirectory Name:")
  (starteam-get-directory-of-files-filtered dir "MGN"))

(defun starteam-get-directory-of-files-filtered (rawdir filter)
  "Get the status of the directory in the current buffer"
  (interactive "DDirectory name:
sFilter [MCONIGU]*")
  (let* ((command "list")
	 (dir (expand-file-name rawdir))
	 (path (starteam-get-starteam-path-from-local-path dir)))
     (message "Checking directory %s (with filter)..." dir )

     (switch-to-buffer (starteam-execute command filter path "-is" "-filter" filter  ) t)
       
     ;;****************************************
     ;;                                        
     ;;  remove the lines of files not needing checkout
     ;;                                        
     ;;****************************************
     (message "Getting rid of all the Current files ... ")
     (save-excursion
       (goto-char(point-min))
       (delete-matching-lines "^Current"))

     (starteam-remove-empty-directory-listings)
     (cd dir)
     (starteam-dired-mode)     
     ))

(defun starteam-diff ()
  "Get the diff of the file in the current buffer"
  (interactive)
  (let ((buf (cond ((eq major-mode 'starteam-log-mode) starteam-log-file)
		    ((eq major-mode 'starteam-dired-mode)
		     (progn (find-file (dired-get-filename)) (current-buffer)))
		    (t (current-buffer)))))
    
    (save-excursion
      (if starteam-log-file (set-buffer starteam-log-file) nil)
      (let* ((temp-file (starteam-checkout-temp-file)))
	(save-buffer)
	(if temp-file
	    (ediff buffer-file-name temp-file)
	  ;;nil
	  (starteam-error-file-not-in-view buffer-file-name) )))
    ))

(defun starteam-emerge-quit ()
  ""
  (if (and (boundp emerge-prefix-argument) (not emerge-prefix-argument))
      (starteam-checkin-file t) nil )
  )

(defun starteam-merge ()
  "Merge the current buffer with the current version"
  (interactive)
  (let* ((temp-file (starteam-checkout-temp-file)))
    
    (save-buffer)
    (add-hook 'quit-hook 'starteam-emerge-quit)
    (if temp-file
	(emerge-files t buffer-file-name temp-file buffer-file-name nil quit-hook)
      ;;nil
    (starteam-error-file-not-in-view buffer-file-name)  )))

(defun starteam-next-action ()
  "Performs the next appropriate action on the file in the current
buffer.

Current Status          Action Performed
--------------          ----------------
Out of Date             starteam-checkout-file
Missing                 starteam-checkout-file
Current                 (message \"Up to date\")
Modified                starteam-checkin-file
Merge                   starteam-merge
Unknown                 starteam-update-status
Not in View             starteam-add"
  (interactive)
  (let ((status (starteam-get-file-status-as-string)))
    (cond ((equal status "Out of Date")
	   (starteam-checkout-file))
	  ((equal status "Missing")
	   (starteam-checkout-file))
	  ((equal status "Current")
	   (message "%s" "Up to date"))
	  ((equal status "Modified")
	   (starteam-checkin-file))
	  ((equal status "Merge")
	   (starteam-merge))
	  ((equal status "Unknown")
	   (starteam-update-status))
	  ((equal status "Not in View")
	   (starteam-add))
	  (t
	   (error "Cannot perform next operation because status is %s" status))
	  )))


(defun starteam-execute (command operation-string path &optional file &rest args)
  "Execute a StarTeam command. 

Returns the name of the buffer into which the output of the 
command was written.

Arguments:
----------
command
     command to send to starteam-executable 
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
     additional arguments to pass to starteam-executable
"
  (if starteam-debug
      (message "Starteam %s operation: [ %s %s %s ]" 
	       operation-string
	       (concat starteam-executable " " command " -x -p \"" 
		       (starteam-get-login-info) "/" path "\"" )
	       (starteam-stringlist-to-single-string args) file)
    nil)
  (let ((bname (concat 
		"*Starteam [" command "] " 
		path 
		"*")))

    (save-excursion
      (get-buffer-create bname)
      (set-buffer bname)
      (toggle-read-only -1)
      (erase-buffer)
      )

    ;(message "starteam executing [%s %s %s]" (concat starteam-executable " " command " -x -p \"" (starteam-get-login-info) "/" path "\"" ) (starteam-stringlist-to-single-string args) file)
    (if args 
	;(apply 'call-process "C:\\winnt\\system32\\cmd.exe" nil bname  nil "/C" "echo" "ECHOING: "
	(apply 'call-process starteam-executable nil bname  nil
	       command
	       "-nologo" ; don't print executable version
	       "-x"      ; non-interactive
	       "-p"
	       (concat (starteam-get-login-info) "/" path)
	       (append (starteam-remove-bad-starteam-args args) (list file))
	       )
      ;(apply 'call-process "C:\\winnt\\system32\\cmd.exe" nil bname nil "/C" "echo" "BOO"
      (apply 'call-process starteam-executable nil bname nil
	     command
	     "-nologo"
	     "-x"
	     "-p"
	     (concat (starteam-get-login-info) "/" path)
	     (if file (list file) nil)
	     )
      )

    (save-excursion
      (set-buffer bname)
      (goto-char (point-min))
      )

  bname))

(define-derived-mode starteam-dired-mode dired-mode starteam-dired-mode-name
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
  ;;(add-hook 'dired-after-readin-hook 'starteam-dired-hook nil t)
;;  (make-local-hook 'dired-mode-hook)
 ;; (add-hook 'dired-mode-hook 'starteam-dired-hook nil t)
  ;; The following is slightly modified from dired.el,
  ;; because file lines look a bit different in vc-dired-mode.
  (set (make-local-variable 'dired-move-to-filename-regexp)
       "\\(\\(.ot in View\\|.ut of Date\\|.urrent\\|.odified\\|.issing\\|.p to Date\\|.erge\\|.nknown\\)[ A-Za-z\t]*r*[w\-]*  *[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+[A-Z ]+[0-9]+ \\)\\|\\(Folder:.*(working dir: \\)")
  (set (make-local-variable 'dired-permission-flags-regexp)
       " [ r][ w] ")
  (set (make-local-variable 'dired-subdir-alist)
       (starteam-dir-get-subdirs))
  (and (boundp 'starteam-dired-switches)
       starteam-dired-switches
       (set (make-local-variable 'dired-actual-switches)
            starteam-dired-switches))
;;  (set (make-local-variable 'starteam-dired-terse-mode) starteam-dired-terse-display)
  (setq vc-dired-mode t))

(defun starteam-dir-get-subdirs ()
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
      
(defun starteam-log-mode (&optional command files buf force)
  "Minor mode for driving version-control tools.
These bindings are added to the global keymap when you enter this mode:
\\[starteam-get-file-history]		display change history of current file
\\[starteam-diff]		show diffs between file versions

While you are entering a change log message for a version, the following
additional bindings will be in effect.

\\[starteam-finish-logentry]	proceed with check in, ending log message entry

"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map starteam-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'starteam-log-mode)
  (setq mode-name "StarTeam-Log")

  (make-local-variable 'starteam-checkin-file)
  (make-local-variable 'starteam-force-checkin)
  (make-local-variable 'starteam-last-buffer)
  (make-local-variable 'starteam-command)

  (setq starteam-force-checkin force)
  (setq starteam-checkin-files files)
  (setq starteam-last-buffer buf)
  (setq starteam-command command)
  
  (make-local-variable 'starteam-log-version)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'vc-log-mode-hook)
  )


;; ------------------------------------------------------------------
;;                   UTILITY ROUTINES
;; ------------------------------------------------------------------


;; Takes a list of strings and concatenates them with a space between each element
(defun starteam-stringlist-to-single-string (args)
  (let ((firstElement (car args))
	(remainingElements (cdr args)))
    (concat firstElement 
	    (if remainingElements 
		(concat " " (starteam-stringlist-to-single-string remainingElements)) 
	      nil))))

(defun starteam-error-file-not-in-view (path)
  (error "Could not find %s in any known Starteam view" path)
)

;; this is necessary because starteam gets confused if it gets blank
;; arguments sent to it via 'apply.
(defun starteam-remove-bad-starteam-args (arglist)
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

(defun starteam-actual-buffer-current-activity ()
  "Examines the current buffer and, based on the buffer mode, returns
the buffer containing the file. 

Handles special cases such as starteam-log-mode and
starteam-dired-mode. For example, when the point is over a file in
starteam-dired-mode, this method opens the file and returns the buffer
for the file underneath the point instead of returning the dired
buffer."
  (interactive)
  (let* ((buf (cond 
	       
	       ;; starteam-log-file
	       ((eq major-mode 'starteam-log-mode) starteam-log-file)
		    
	       ;; starteam-dired-mode 
	       ;;
	       ;; Note: We have to open the file under the point, get
	       ;; the buffer, then switch back to the dired
	       ;; buffer. Otherwise the buffer in view after the
	       ;; function call would be different than the one in
	       ;; view before the call.
	       ((eq major-mode 'starteam-dired-mode) 
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

(defun starteam-current-buffer-dir-and-file ()
  "Examines the current buffer and returns the appropriate file directory name 
based on the buffer mode. Values are returned as a list in the form 

     (directory file)

Calls 'error if the current buffer is not associated with a file."
  (interactive)
  (let* ((buf (starteam-actual-buffer-current-activity))
	 (file (file-name-nondirectory (buffer-file-name buf)))
	 (bfilename (buffer-file-name buf))
	 (dir))
    
    (if bfilename
	(setq dir (file-name-directory bfilename))
      (error "Buffer \"%s\" not associated with a file" (current-buffer)))
    (list dir file)
    ))

;; NOTE: this method is not very conservative. For example, assume
;; starteam-to-directory-alist looks like this:
;;
;;  (setq starteam-to-directory-alist (list (cons  "^X:/test"  "test/chrisview" )))
;;
;; then (starteam-get-starteam-path-from-local-path "X:/test/a/b") --> "test/chrisview/a/b"
;; which is correct. 
;; 
;; Unfortunately, (starteam-get-starteam-path-from-local-path "X:/test27/a/b") --> "test/chrisview27/a/b"
;; which is incorrect; instead it should have signalled an error
;;
(defun starteam-get-starteam-path-from-local-path (local-dir)
  "Given a local directory, attempts to convert it to a starteam path
based upon the values in starteam-to-directory-alist. If the given directory
cannot be reconciled to a starteam path, 'error is called.

For example, assume that starteam-to-directory-alist is set up as follows:

 (setq starteam-to-directory-alist (list (cons  \"^X:/test\"  \"test/myview\" )))

In this case

 (starteam-get-starteam-path-from-local-path \"X:/test/a/b\") 

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
	    starteam-to-directory-alist)
    (if (not path)
	(error "Could match local path %s to path in any known Starteam view" local-dir)
      path)
    ))

(defun starteam-remap-cygdrive (path)
  "If the provided path contains \"/cygdrive/letter/\", strip those out and replace
with \"letter:\"
"
  (string-replace-match "^/cygdrive/\\([^/]\\)\\(.*\\)" path "\\1:\\2")
)

(defun starteam-get-working-dir-from-local-path (local-dir)
  "Given a local directory, attempts to determine the starteam working directory
based upon the values in starteam-to-directory-alist. If the given directory
cannot be reconciled to a starteam path, 'error is called.

For example, assume that starteam-to-directory-alist is set up as follows:

 (setq starteam-to-directory-alist (list (cons  \"^X:/test\"  \"test/myview\" )))

In this case, both

 (starteam-get-working-dir-from-local-path \"X:/test/a/b/foo.java\") 

and

 (starteam-get-working-dir-from-local-path \"X:/test/g/bar.c++\") 

return \"X:/test\"
"
(if starteam-map-cygdrive
      (starteam-remap-cygdrive (starteam-get-working-dir-from-local-path-no-map local-dir))
      (starteam-get-working-dir-from-local-path-no-map)
  )
)

(defun starteam-get-working-dir-from-local-path-no-map (local-dir)
  "Perform the starteam-get-working-dir-from-local-path before applying any cygdrive mappings"
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
	    starteam-to-directory-alist)
    (if (not working-dir)
	(error "Could match local path %s to path in any known Starteam view" local-dir)
      working-dir)
    ))

(defun starteam-output-buffer-contains (buffer regexp)
  "Searches the given buffer for the occurence of the given regular 
expression; returns t if the regexp was found, and nil otherwise."
  (if starteam-debug (message "Searching buffer %s for regexp %s" buffer regexp))
  (save-excursion 
    (set-buffer buffer)
    (goto-char (point-min))      
    (if (re-search-forward regexp nil t) 
	t 
      nil) 
    ))

(defun starteam-display-buffer (buffer)
  "Displays the given buffer according to the value of starteam-switch-to-output"
  (cond ((equal starteam-switch-to-output t)   (switch-to-buffer buffer))
	((equal starteam-switch-to-output nil) nil)
	(t (shrink-window-if-larger-than-buffer (display-buffer buffer t)))
	))

(defun starteam-print-message-file-status ()
  "Checks the status of the file in the current buffer and 
prints it to the mini-buffer"
  (interactive)
  (message "Status of %s is: %s" (buffer-file-name (starteam-actual-buffer-current-activity)) (starteam-get-file-status-as-string)))

(defun starteam-menubar-setup ()
  "Adds a menu to the menu bar containing starteam-mode operations"
  (interactive)
  
  ;;;(define-key-after menu-bar-tools-menu [starteam] (cons "STARTEAM" starteam-menu-map) 'vc) 
  (define-key global-map [menu-bar starteam] (cons "STARTEAM" starteam-menu-map))

  (define-key starteam-menu-map [starteam-dired-with-filters] '("List Directory w/ Filters" . starteam-get-directory-of-files-filtered))
  (define-key starteam-menu-map [starteam-dired]      '("List Directory" . starteam-get-directory))
  (define-key starteam-menu-map [separator4]          '("--"))
  (define-key starteam-menu-map [starteam-update-status] '("Update status" . starteam-update-status))
  ;; disabled merge until merge bug is fixed
  ;;(define-key starteam-menu-map [starteam-merge]      '("Merge" . starteam-merge))
  (define-key starteam-menu-map [starteam-add]        '("Add" . starteam-add))
  (define-key starteam-menu-map [separator3]          '("--"))
  (define-key starteam-menu-map [starteam-status]     '("Show Status" . starteam-print-message-file-status))
  (define-key starteam-menu-map [starteam-history]    '("Show History" . starteam-get-file-history))
  (define-key starteam-menu-map [starteam-diff]       '("Show Differences" . starteam-diff))
  (define-key starteam-menu-map [separator2]          '("--"))
  (define-key starteam-menu-map [starteam-checkout]   '("Check Out" . starteam-checkout-file))
  (define-key starteam-menu-map [starteam-checkin]    '("Check In" . starteam-checkin-file))
  (define-key starteam-menu-map [separator1]          '("--"))
  (define-key starteam-menu-map [starteam-unlock]     '("Unlock" . starteam-unlock-file))
  (define-key starteam-menu-map [starteam-lock]       '("Lock (exclusive)" . starteam-lock-file))
  )


(define-key starteam-dired-mode-map "\C-xv" vc-prefix-map)
(define-key starteam-dired-mode-map "v" vc-prefix-map)
(define-key starteam-dired-mode-map "g" 'starteam-dired-revert)

(global-set-key "v=" (quote starteam-diff))
(global-set-key "vm" (quote starteam-merge))
(global-set-key "vv" (quote starteam-next-action))
(global-set-key "vl" (quote starteam-get-file-history))
(global-set-key "v\C-l" (quote starteam-lock-file))
(global-set-key "v\C-u" (quote starteam-unlock-file))
(global-set-key "vd" (quote starteam-get-directory))
(global-set-key "v" (quote starteam-get-directory-of-files-to-checkout))
(global-set-key "v	" (quote starteam-get-directory-of-files-to-checkin))


(defvar starteam-log-entry-mode nil)
(defvar starteam-log-file nil)
(defvar starteam-log-version nil)

;; Initialization code, to be done just once at load-time
(if starteam-log-entry-mode
    nil
  (setq starteam-log-entry-mode (make-sparse-keymap))
  (define-key starteam-log-entry-mode "\C-c\C-c" 'starteam-finish-logentry)
  )



(add-hook 'ediff-cleanup-hook '(lambda nil
				 (bury-buffer ediff-buffer-B)))
