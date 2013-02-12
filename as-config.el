
;; Load our project specific options, if available.
(load (concat (getenv "PWD") "/.project-config.el") t)

;; We need to make an adjustment to hideshow to work properly with AS syntax.
(setq hs-special-modes-alist (cons '(actionscript-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning) hs-special-modes-alist))
(add-hook 'actionscript-mode-hook 'hs-minor-mode)

;; The documentation for perform-replace is wrong. Here is the correct interface:
;; (defun perform-replace (from-string replacements query-flag regexp-flag delimited-flag &optional repeat-count map start end)

;; Note that the regexps are tried in the order they were added to the list,
;; so you should add the more specific ones first.

;; Default

(defun insert-flash-boilerplate()
  "When we open a new AS file, automatically insert some boilerplate
code. If the filename starts with 'S_', then we will insert a template
for our StateMachine type. This function expects that your AS root starts
with a folder named 'as' from which it builds package names. Finally, this
this function requires the necessary templates to exist in ~ .emacs.d/site-lisp"
  ;; TODO: Generalize this so that we can add new filename patterns and
  ;;       functions to run when those files are created.

  (let* ((wholePath buffer-file-name)
	 (filename (file-name-nondirectory wholePath)))

    ;; if this file has the "as" extension
    (if (equal (file-name-extension filename) "as")

	(let ((className (file-name-sans-extension filename))
	      (package-regexp (concat "^.*?as/\\(.*?\\)" filename)))

	  ;; We use a regexp to grab everything in the path between the as (root) folder and this file.
	  ;; We'll break this apart to get a list of package names.
	  (if (string-match package-regexp wholePath)
	      (let ((modified-path (match-string 1 wholePath)))
		;; Parse the path into package names.
		(let ((package-list (split-string modified-path "/"))
		      (package-string "")
		      (prefix-regexp "^..") ; first two characters
		      (file-prefix ""))

		  ;; combine the package names into a string
		  (dolist (elt package-list)
		    (setf package-string (concat package-string elt ".")))

		  ;; Check for a filename prefix.
		  (string-match prefix-regexp filename)

		  (setq file-prefix (match-string 0 filename))

		  (cond
		   ((equal file-prefix "S_")
		    ;; This filename matches our convention for FSM states.
		    (insert-file-contents "~/.emacs.d/site-lisp/State_Template")
		    ;; Replace our template tokens.
		    (perform-replace "#CLASSNAME#" className nil nil nil nil nil (point-min) (point-max))
		    (perform-replace "#PACKAGE_STRING#" package-string nil nil nil nil nil (point-min) (point-max)))
		   (t
		    ;; Normal file
		    ;; TODO: Put this into a template file.
		    (insert (concat "class " package-string className "{\n\n\tfunction " className "(){\n\n\t}\n\n\tpublic function toString(Void):String{\n\n\t\treturn \"" className "()\";\n\t}\n}")))))))))))

(add-hook 'find-file-not-found-hooks 'insert-flash-boilerplate)

(defun pt-output-sfx-data (external?)
  (interactive 
   (list
    (y-or-n-p "External? ")))
  (let ((marked-files (dired-get-marked-files))
	(external (if external? "true" "false")))
    (setq marked-files (map 'list 'file-name-nondirectory marked-files))
    (princ "[\n")
    (loop for f in marked-files do
	  (princ (format "{id: \"%s\", copies: 1, path: \"%s\", external: %s},\n" (file-name-sans-extension f) f external)))
    (princ "];\n")))

(defun copy-function-with-new-sig(src-buffer cur-sig dest-buffer new-sig)
  (set-buffer src-buffer)
  (goto-char (point-min))
  (if (re-search-forward cur-sig nil t)
      (let ((start (point))
	    (end (progn (backward-char) (forward-list) (point))))
	(let ((function-body (buffer-substring start end)))
	  ;; if there's nothing in this function body or there's only a 'return false', skip it.
	  (if (equal 0 (string-match "[ \t\n]*\\(return false;\\)?[ \t\n]*}" function-body))
	      (progn
		;;(message (format "Empty function found: %s" function-body))
		nil)
	    (progn
	      ;;(message (format "Function found: %s" function-body))
	      ;; Make some substitutions
	      (let ((new-function (with-temp-buffer
				    (insert "\t" new-sig function-body "\n\n")
				    (perform-replace "_parent_fsm." "" nil nil nil nil nil (point-min) (point-max))
				    (perform-replace "_owner." "" nil nil nil nil nil (point-min) (point-max))
				    (buffer-string))))

		(set-buffer dest-buffer)
		(insert new-function))
	      t))))
    nil))

(defun convert-fsm(src-buffer dest-buffer)
  (interactive "bSource buffer:\nbDestination buffer:")
  (let ((src-buffer (get-buffer src-buffer))
	(dest-buffer (get-buffer dest-buffer)))
    (let ((src-classname (as-get-classname src-buffer)))
      ;; Discard any leading "S_" from the classname.
      (when (string= "S_" (substring src-classname 0 2))
	(setq src-classname (substring src-classname 2)))
      (set-buffer dest-buffer)
      (goto-char (point-min))
      (re-search-forward "function InitStateMachine" nil t)
      (forward-line -1)
      (c-indent-command)
      (insert "//----------------//\n")
      (c-indent-command)
      (insert (format "// %s\n" src-classname))
      (let ((new-enter-name (concat src-classname "_OnEnter"))
	    (new-message-name (concat src-classname "_OnMessage"))
	    (new-update-name (concat src-classname "_OnUpdate"))
	    (new-exit-name (concat src-classname "_OnExit")))
	(let ((new-enter-sig (format "public function %s(Void):Void{" new-enter-name))
	      (new-message-sig (format "public function %s(msg:Msg_Object):Boolean{" new-message-name))
	      (new-update-sig (format "public function %s(Void):Boolean{" new-update-name))
	      (new-exit-sig (format "public function %s(Void):Void{" new-exit-name)))
	  (let ((OnEnter-found (copy-function-with-new-sig src-buffer (as-get-function-re "OnEnter") dest-buffer new-enter-sig))
		(OnMessage-found (copy-function-with-new-sig src-buffer (as-get-function-re "OnMessage") dest-buffer new-message-sig))
		(OnUpdate-found (copy-function-with-new-sig src-buffer (as-get-function-re "OnUpdate") dest-buffer new-update-sig))
		(OnExit-found (copy-function-with-new-sig src-buffer (as-get-function-re "OnExit") dest-buffer new-exit-sig)))
	    ;; Add the state info 
	    (when (or OnEnter-found OnMessage-found OnUpdate-found OnExit-found)
	      (with-current-buffer dest-buffer
		(if (re-search-forward "return fsm;" nil t)
		    (progn 
		      (forward-line -1)
		      (c-indent-command)
		      (insert "fsm.AddState(\"" src-classname "\",\n")
		      (c-indent-command)
		      (if OnEnter-found
			  (insert new-enter-name)
			(insert "null"))
		      (insert ",\n")
		      (c-indent-command)
		      (if OnMessage-found
			  (insert new-message-name)
			(insert "null"))
		      (insert ",\n")
		      (c-indent-command)
		      (if OnUpdate-found
			  (insert new-update-name)
			(insert "null"))
		      (insert ",\n")
		      (c-indent-command)
		      (if OnExit-found
			  (insert new-exit-name)
			(insert "null"))
		      (insert ");\n\n")))))))))))

(defun as-make-cfg-func(props)
  ;;(print (format "props: %s" props))
  (lexical-let ((props props))
    (lambda ()
      (interactive)
      (dolist (p props)
	(let ((prop (first p))
	      (val (second p)))
	  (put 'actionscript-mode prop val))))))

(defun as-collect-props (id cfg-lst)
  ;;(print (format "as-collect-props (%s %s)\n" id cfg-lst))
  (let* ((cur (assoc id cfg-lst))
	 (section-props (second cur))
	 (my-props (second (assoc :props section-props)))
	 (my-parents (second (assoc :super section-props)))
	 (all-props '()))
    ;; Iterate through each of id's parents.
    (dolist (parent my-parents)
      (setf all-props (append all-props (as-collect-props parent cfg-lst))))
    (append all-props my-props)))

(defmacro as-project-config (&rest cfg)
  ;; Take the given config and generate
  ;; a closure and attach it to the given
  ;; keybinding(s).

  (dolist (section cfg)
    (let* ((section-props (second section))
	   (key (second (assoc :key section-props)))
	   (commands (second (assoc :commands section-props))))
      (when key
	(let ((all-props (as-collect-props (car section) cfg)))
	  (global-set-key key (as-make-cfg-func all-props commands)))))))

(defun as-set-env (env)
  (interactive (list
		(read-from-minibuffer "Which env? " nil nil t)))
  (let* ((section (assoc env *PROJECT-CONFIG*))
	 (all-props (as-collect-props (car section) *PROJECT-CONFIG*)))
    (funcall (as-make-cfg-func all-props))))

;; ------------------------------------------------------------------ 

(defun update-etags(source-folder)
  (interactive
   (list
    (get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))))
  (save-some-buffers)
  (let ((wd (getenv "PWD")))
    (shell-command (concat "find " source-folder " -name '*.as' -print | etags --language=none --regex='/[ \t]*\\(?:static \\)?\\(?:private \\)?\\(?:public \\)?\\(?:static \\)?function \\([a-zA-Z0-9_]+\\)/\\1/' -o " wd "/TAGS -"))
    (visit-tags-table (concat wd "/TAGS"))))

(defun as-print-func-info()
  "Insert a print statement immediately after the nearest function definition before point."
  (interactive)
  (save-excursion
    (re-search-backward as-function-re)
    (goto-char (match-end 0))
    (let ((function (match-string 4))
	  (args (match-string 5))
	  (static1 (match-string 2))
	  (static2 (match-string 3))
	  (arg-string "")
	  (debug-msg "")
	  (first-part "\"")
	  (arg-trace "\""))

      (when (> (length args) 0)
	;;parse args
	(setf arg-string (mapconcat (function (lambda (x)
						;; chop off any type info
						(car (split-string x ":"))))
				    (split-string args ",")
				    " +\", \"+ ")))

      ;; See if we should add "this."
      (if (not (or (stringp static1) (stringp static2)))
	  (setf first-part "this + \"."))

      ;; Check if there are any args to trace
      (when (and (> (length args) 0)
		 (not (equal arg-string "Void")))
	(setf arg-trace (concat " : \" + " arg-string)))

      ;;now, print out our debug statement after the function start
      (setf debug-msg (concat "trace" "(" first-part function "(" args ")" arg-trace ");"))
      (insert (concat "\n\n" debug-msg))
      (indent-according-to-mode)
      (message debug-msg))))

(defun as-insert-trace ()
  "Insert an empty trace call at point. If we are over a word, then trace that word on the next line"
  (interactive)

  (let ((cw (current-word)))
    (cond
     ((not (equal cw ""))
      ;; goto the next line
      (end-of-line)
      (insert (format "\n%s(\"%s: \" + %s);" "trace" cw cw))
      (indent-according-to-mode))
     (t
      (indent-according-to-mode)
      (insert (format "%s(\"\");" "trace"))
      (backward-char 3)))))

(defun get-all-files(file_regexp lst)
  (if (null lst)
      nil
    (let* ((file (car lst))
	   (basename (file-name-nondirectory file)))
      ;; if the first item is a dir
      (if (and (file-directory-p file)
	       (not (equal basename "."))
	       (not (equal basename "..")))
	  ;; get all the files in this dir
	  (let ((new_list (get-all-files file_regexp (directory-files file t nil t))))
	    ;; if there were files in it...
	    (if new_list
		;; Add the dir name as the first element of the new list, and add
		;; that as the first element to whatever is left.
		(cons (cons basename new_list) (get-all-files file_regexp (cdr lst)))
	      nil))
	;; not a dir. Check if it matches the regexp.
	(if (string-match file_regexp basename)
	    ;; Match. Add on to whatever is left.
	    (cons basename (get-all-files file_regexp (cdr lst)))
	  ;; No match. Move on the the rest of the list.
	  (get-all-files file_regexp (cdr lst)))))))

(defun as-print(msg)
  (princ msg)
  (terpri))

(defun as-preprocess(src_folder build_folder file_regexp cpp-args)

  ;; Make sure that src_folder exists.
  ;; If it doesn't, exit.
  (if (not (file-directory-p src_folder))
      (let ((msg (format "as-preprocess: Source folder: %s does not exist!!!" src_folder)))
	(as-print msg)
	(message msg))
    ;; Recursively travel the directory structure.
    (let ((files (directory-files src_folder t nil t)))
      (dolist (f files)
	(let ((basename (file-name-nondirectory f)))
	  ;; If this is a directory, and is not "." or ".."
	  (if (and (file-directory-p f)
		   (not (equal basename "."))
		   (not (equal basename "..")))
	      (as-preprocess f (concat build_folder "/" basename) file_regexp cpp-args)
	    ;; f is not a directory. Check if it matches
	    ;; the regexp.
	    (when (string-match file_regexp basename)
	      ;; Match
	      ;;(as-print (format "processing: %s\n" basename))
	      ;; Create build folder, if it doesn't exist.
	      (when (not (file-directory-p build_folder))
		(as-print (format "Build folder does not exist. Creating %s" build-folder))
		(make-directory build_folder t))
	      (let ((build-file (concat build_folder "/" basename)))
		;; Check if src is newer than build
		;; *** We aren't performing this check right now because the resolution
		;;     on the timer is only to the minute.
		;;(if (file-newer-than-file-p f build-file)
		(progn
		  (as-print (format "Processing: %s" basename))
		  (as-print (shell-command-to-string (format "cpp %s -P %s %s" cpp-args f build-file))))))))))))

(defun get-or-ask(s p q)
  (if (memq p (symbol-plist s))
      (get s p)
    (eval q)))

(defvar *as-debug-buffer* "*as-tracer*")
(defvar *as-profiler-buffer* "*as-profiler*")

(defun launch-flash-tracer ()
  (interactive)
  (when (get-buffer *as-debug-buffer*)
    ;; Clear the buffer.
    (save-current-buffer
      (set-buffer *as-debug-buffer*)
      (erase-buffer)))
  (unless (get-process "as-tracer")
    (message "Starting as-tracer...")
    (start-process "as-tracer" *as-debug-buffer* "sbcl" "--core" "/home/astro/projects/web/flash-tracer/flash-tracer.core" "--noinform")
    (message "Done.")))

(defun as-profiler-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun launch-flash-profiler ()
  (interactive)
  (when (get-buffer *as-profiler-buffer*)
    ;; Clear the buffer.
    (save-current-buffer
      (set-buffer *as-profiler-buffer*)
      (erase-buffer)))
  (unless (get-process "as-profiler")
    (message "Starting as-profiler...")
    (let ((p (start-process "as-profiler" *as-profiler-buffer* "sbcl" "--core" "/home/astro/projects/web/flash-profiler/flash-profiler.core" "--noinform")))
      (set-process-filter p 'as-profiler-insertion-filter))
    (message "Done.")))

(defun show-dev-screens ()
  (let ((cb *as-debug-buffer*))
    (switch-to-buffer-other-window "*as-build*")
    (goto-char (point-max))
    (switch-to-buffer-other-window cb)))

(defun as-mtasc(main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)
  "Compile a swf using mtasc."
  (let ((compile-string (format "mtasc -main -cp ~/Software/Classes -mx -cp ~/Software/mtasc/ocaml/mtasc/std -cp %sas -swf %s%s -frame %s -trace %s" build-folder build-folder swf-name mtasc-frame custom-trace)))

    (unless use-existing-swf
      (setq compile-string (format "%s -header %s:%s:%s" compile-string swf-width swf-height swf-frame-rate)))

    (when using-flash8
      (setq compile-string (format "%s -version 8 -cp ~/Software/mtasc/ocaml/mtasc/std8" compile-string)))

    (setq compile-string (format "%s %s" compile-string main-file))

    (as-print (format "Compile command: %s" compile-string))

    ;; Run MTASC. Return the output.
    (shell-command-to-string compile-string)))

(defun as-ediff-list (file-A file-B)
  (require 'ediff)
  (let (buffer-A buffer-B)
    (message "Reading file %s ... " file-A)
    (ediff-find-file 'file-A 'buffer-A 'ediff-last-dir-A nil)
    (message "Reading file %s ... " file-B)
    (ediff-find-file 'file-B 'buffer-B 'ediff-last-dir-B nil)

    ;; ediff-convert-standard-filename puts file names in the form appropriate
    ;; for the OS at hand.
    (setq file-A (ediff-convert-standard-filename (expand-file-name file-A)))
    (setq file-B (ediff-convert-standard-filename (expand-file-name file-B)))

    (setq ediff-buffer-A buffer-A
	  ediff-buffer-B buffer-B
	  ediff-buffer-C nil)

    (setq ediff-diff-buffer
	  (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*")))

    (ediff-make-diff2-buffer ediff-diff-buffer file-A file-B)
    (ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
    (setq ediff-diff-options "-E --tabsize=1")
    (let ((result (cdr (ediff-extract-diffs	ediff-diff-buffer nil))))
      (kill-buffer ediff-diff-buffer)
      (kill-buffer "*ediff-errors*")
      result)))

(defun find-mtasc-error(mtasc-output build-folder source-folder)

  (let ((error-regexp (concat "^" (expand-file-name build-folder) "\\([\\/_[:alnum:]]*\.as\\):\\([0-9]*\\): characters \\([0-9]*\\)-\\([0-9]*\\) : \\(.*\\)")))

    (as-print "Parsing error output...")
    (as-print mtasc-output)

    ;; if there was a match...
    (if (string-match error-regexp mtasc-output)
	(let ((error-path (match-string 1 mtasc-output))
	      (error-line (string-to-number (match-string 2 mtasc-output)))
	      (error-start-char (string-to-number (match-string 3 mtasc-output)))
	      (error-end-char (string-to-number (match-string 4 mtasc-output)))
	      (error-msg (match-string 5 mtasc-output)))

	  (as-print error-path)

					;;;; Because of the preprocessor, the build and source files
					;;;; may differ. Therefore, we execute the following steps:

						;;; The biggest difficulty with finding the correct match is that
						;;; preprocessor directives can appear anywhere in the source, and
						;;; cause discrepencies between the src and build. Fortunately,
						;;; these discrepencies are isolated, so we should be able to work
						;;; around them. In the worst case, the offending text occurs within
						;;; a cpp directive. In that case we should still be able to identify
						;;; that position as the source of the error.

						;;; To overcome this problem, we use ediff to find all the regions of 
						;;; similar text. 

	  (let ((source-buffer nil)
		(build-buffer nil)
		(offending-text nil)
		(offending-start-pos nil)
		(offending-end-pos nil)
		(diff-list (as-ediff-list (concat build-folder error-path) (concat source-folder error-path))))

						;;; 1. Find the error in the build file.
	    (setq build-buffer (find-file-read-only (concat build-folder error-path)))
	    (goto-line error-line)
	    (forward-char error-start-char)

						;;; 2. Record the offending text listed in the error.
	    (setq offending-start-pos (point))
	    (setq offending-end-pos (+ (point) (- error-end-char error-start-char)))
	    (setq offending-text (buffer-substring-no-properties offending-start-pos offending-end-pos))

	    (as-print (format "Offending text: %s" offending-text))
	    (as-print (format "line: %s" error-line))
	    ;;(as-print (format "start: %s" offending-start-pos))

						;;; 3. Open the source file
	    (setq source-buffer (find-file (concat source-folder error-path)))
	    (set-mark (point))
	    (beginning-of-buffer)

						;;; 4. Search for matching text in the source file.

	    ;; Iterate through diff-list until we find a max value for a build region that is greater than
	    ;; offending-start-pos.

	    ;; Compute offending-start-pos - build-region-min and add that value to src-region-min.
	    
	    ;;(as-print diff-list)
	    ;;(as-print (length diff-list))

	    (let ((error-src-start-pos
		   (do* ((i 0 (1+ i))
			 (diff-region (nth i diff-list) (nth i diff-list)))
		       ((<= offending-start-pos (aref diff-region 1)) ;; test
			(+ (- offending-start-pos (aref diff-region 0)) (aref diff-region 2))))))

							;;; 5a. Highlight the text.
	      (let ((best-match-start-pos error-src-start-pos)
		    (best-match-end-pos (+ error-src-start-pos (length offending-text))))

		(as-print (format "start pos: %s" best-match-start-pos))
		(as-print (format "end pos: %s" best-match-end-pos))
		(as-print "\n")
		(as-print error-msg)
		(message error-msg)

		(setq mtasc-error-overlay (make-overlay best-match-start-pos best-match-end-pos))
		(overlay-put mtasc-error-overlay 'face '(background-color . "#6a0000"))

		;; put point at the start of the error
		(goto-char best-match-start-pos)

		;; Show block if hidden by hideshow.
		(save-excursion
		  (hs-show-block))

		;; close the build file
		(kill-buffer build-buffer)))))

										;;; 5b. If we cannot find a match in the source file, the
										;;;     the error probably occurs in a macro expansion. In
										;;;     this case, we will highlight the error in the build
										;;;     file.
      ;; 							(as-print "Could not find error in source file.")
      ;; 							(switch-to-buffer build-buffer)
      ;; 							(setq mtasc-error-overlay (make-overlay offending-start-pos offending-end-pos))
      ;; 							(overlay-put mtasc-error-overlay 'face '(background-color . "#3a0000"))))

      ;; There was an error, but we couldn't parse the output.
      (as-print "\n")
      (as-print (format "UNKNOWN ERROR: %s" mtasc-output))
      (message mtasc-output))

    (let ((cb (current-buffer)))
      (switch-to-buffer-other-window "*as-build*")
      (goto-char (point-max))
      (switch-to-buffer-other-window cb))))

(defun as-flash-compile(source-folder build-folder swf-name swf-width swf-height swf-frame-rate main-file custom-trace use-existing-swf using-flash8 server-path copy-command launch-browser cpp-args browser-target mtasc-frame)
  "Compile Flash movie. Reload in browser. Check for errors."

  (interactive
   (list
    (get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
    (get-or-ask 'actionscript-mode 'build-folder '(read-file-name "Which is the build folder?" "src"))
    (get-or-ask 'actionscript-mode 'swf-name '(read-string "What is the swf's name?" "fireworks.swf"))
    (get-or-ask 'actionscript-mode 'swf-width '(read-string "[mtasc] Width?" "360"))
    (get-or-ask 'actionscript-mode 'swf-height '(read-string "[mtasc] Height?" "558"))
    (get-or-ask 'actionscript-mode 'swf-frame-rate '(read-string "[mtasc] Frame Rate?" "24"))
    (get-or-ask 'actionscript-mode 'main-file '(read-string "[mtasc] Main file?"))
    (get-or-ask 'actionscript-mode 'custom-trace '(read-string "[mtasc] Custom trace method?"))
    (get-or-ask 'actionscript-mode 'use-existing-swf '(y-or-n-p "[mtasc] Use existing swf?"))
    (get-or-ask 'actionscript-mode 'using-flash8 '(y-or-n-p "[mtasc] Using Flash 8?"))
    (get-or-ask 'actionscript-mode 'server-path '(read-string "What is the path to the server?" "local"))
    (get-or-ask 'actionscript-mode 'copy-command '(read-string "Copy command"))
    (get-or-ask 'actionscript-mode 'launch-browser '(y-or-n-p "Launch Browser?"))
    (get-or-ask 'actionscript-mode 'cpp-args '(read-string "cpp args?"))
    (get-or-ask 'actionscript-mode 'browser-target '(read-string "browser target"))
    (get-or-ask 'actionscript-mode 'mtasc-frame '(read-string "[mtasc] frame?"))))

  (save-some-buffers)

  (let ((standard-output (get-buffer-create "*as-build*")))

    (let ((wd (getenv "PWD")))

      (with-current-buffer (get-buffer "*as-build*")
	(goto-char (point-max)))

      (as-print "\n---Starting Build---\n")

      ;; run the preprocessor
      (as-print "Running preprocessor...")
      (as-print "-----------------------")
      (as-preprocess source-folder build-folder "^.*\.as$" cpp-args)

      ;; copy a clean swf over
      ;; (only if use-existing-swf)
      (when use-existing-swf
	(as-print (format "\nCopying swf: %s" swf-name))
	(as-print "--------------")
	(let ((original-swf-name (get 'actionscript-mode 'swf-original-name)))
	  (copy-file (concat source-folder original-swf-name) (concat build-folder swf-name) t)))

      ;; copy any additional files over, if newer than build versions.
      (as-print "\nCopying additional files...")
      (as-print "---------------------------")
      (as-print (shell-command-to-string (format "rsync --update --cvs-exclude --exclude \"*.as\" --archive --verbose %s %s" source-folder build-folder)))

      ;; create the html page, if necessary.
      (let ((html-page (concat build-folder "index.html")))
	(unless (file-exists-p html-page)
	  (with-temp-file html-page
	    (insert-file-contents "/home/astro/.emacs.d/site-lisp/swf_html_template")
	    (perform-replace "#TITLE#" swf-name nil nil nil nil nil (point-min) (point-max))
	    (perform-replace "#WIDTH#" swf-width nil nil nil nil nil (point-min) (point-max))
	    (perform-replace "#HEIGHT#" swf-height nil nil nil nil nil (point-min) (point-max))
	    (perform-replace "#SWFNAME#" swf-name nil nil nil nil nil (point-min) (point-max)))))

      ;; run mtasc
      (as-print "\nRunning mtasc...")
      (as-print "----------------")
      (let ((mtasc-output (as-mtasc main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)))

	;; delete any error overlays
	(when (boundp 'mtasc-error-overlay)
	  (delete-overlay mtasc-error-overlay))

	(if (equal mtasc-output "")
	    ;; everything must have gone ok.
	    (progn
	      ;; Catch debug messages

	      (launch-flash-tracer)
	      (launch-flash-profiler)

	      (unless (equal server-path "local")
		;; copy this file over to our staging
		(as-print "\nCopying files to staging server...")
		(as-print "----------------------------------")
		(as-print (format "[%s]" server-path))
		(let ((copy-output (shell-command-to-string (concat copy-command build-folder "* " server-path))))
		  (as-print (format "%s" copy-output))))

	      ;; Launch browser (if required).
	      (when launch-browser
		(if (equal server-path "local")
		    (browse-url (concat "file://" (expand-file-name build-folder) "index.html"))
		  (browse-url browser-target)))

	      (as-print (current-time-string))
	      (as-print "Success!")
	      (message "Success!")

					;(sleep-for 2)	; if we don't do this, then no debug messages will show up

	      ;; Show the trace output in one window, and
	      ;; show the build messages in the other.
	      ;; TODO: Simplify this.
	      (show-dev-screens)

	      (when (memq 'on-finish-hook (symbol-plist 'actionscript-mode))
		(let ((ofh (get 'actionscript-mode 'on-finish-hook)))
		  ;; Make sure it isn't nil.
		  (when ofh
		    (funcall ofh)))))

	  ;; Had output. Must be error.
	  ;; Parse error message.
	  (find-mtasc-error mtasc-output build-folder source-folder))))))

(defun as-path-to-current-file ()

  (let* ((wholePath buffer-file-name)
	 (filename (file-name-nondirectory wholePath))
	 (className (file-name-sans-extension filename)))

    ;; We use a regexp to grab everything in the path between the as (root) folder and this file.
    (let ((package-regexp (concat "^.*?as/\\(.*?\\)" filename)))

      ;; We'll break this apart to get a list of package names.
      (if (string-match package-regexp wholePath)
	  (let ((package-path (match-string 1 wholePath)))
	    ;; Parse the path into package names.
	    (let ((package-list (split-string package-path "/"))
		  (package-string ""))

	      ;; combine the package names into a string
	      (dolist (elt package-list)
		(setf package-string (concat package-string elt ".")))
	      (concat package-string className)))
	nil))))

(defun as-quick-compile (source-folder custom-trace cpp-args)
  "Compile the current buffer into a temporary swf and launch the browser. This function
is intended for running quick tests of code."

  (interactive
   (list
    (get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
    (get-or-ask 'actionscript-mode 'custom-trace '(read-string "[mtasc] Custom trace method?"))
    (get-or-ask 'actionscript-mode 'cpp-args '(read-string "cpp args?"))))

  (save-buffer)

  (let ((standard-output (get-buffer-create "*as-build*"))
	(browse-url-browser-function #'browse-url-netscape))

    (with-current-buffer (get-buffer "*as-build*")
      (goto-char (point-max)))

    (as-print "\n---Starting Build---\n")

    (let ((main-file "/tmp/as-quick-compile/Quick_Compile_Main.as")
	  (build-folder "/tmp/as-quick-compile/")
	  (swf-name "temp.swf")
	  (mtasc-frame 1)
	  (swf-width 100)
	  (swf-height 100)
	  (swf-frame-rate 24)
	  (use-existing-swf nil)
	  (using-flash8 t))

      ;; run the preprocessor
      (as-print "Running preprocessor...")
      (as-print "-----------------------")
      (as-preprocess source-folder build-folder "^.*\.as$" cpp-args)
      (as-print "\n")

			;;;; Create the generic main class file.
      ;; Are we compiling the whole class, or just some "free" code?

      ;; We need to get the path to this file.
      (let ((code-to-inject (concat (as-path-to-current-file) ".test()")))
	(with-temp-file (concat build-folder "Quick_Compile_Main.as")
	  (insert-file-contents "/home/astro/.emacs.d/site-lisp/Quick_Compile_Template")
	  (perform-replace "#CODE#" code-to-inject nil nil nil nil nil (point-min) (point-max))))

      ;; run mtasc
      (as-print "\nRunning mtasc...")
      (as-print "----------------")
      (let ((mtasc-output (as-mtasc main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)))

	;; delete any error overlays
	(when (boundp 'mtasc-error-overlay)
	  (delete-overlay mtasc-error-overlay))

	(if (equal mtasc-output "")
	    ;; everything must have gone ok.
	    (progn
	      ;; Catch debug messages
	      (launch-flash-tracer)

	      ;; Launch browser.
	      (browse-url (concat "file://" (expand-file-name build-folder) swf-name))

	      (as-print "Success!")
	      (message "Success!")

	      (sleep-for 2)	; if we don't do this, then no debug messages will show up

	      ;; Show the trace output in one window, and
	      ;; show the build messages in the other.
	      ;; TODO: Simplify this.
	      (show-dev-screens))

	  ;; Had output. Must be error.
	  ;; Parse error message.
	  (find-mtasc-error mtasc-output build-folder source-folder))))))

(defun as-insert-flash-func()
  (interactive)
  (let ((isStatic (y-or-n-p "Static? "))
	(isPublic (y-or-n-p "Public? "))
	(f-string ""))

    (when isStatic
      (setf f-string "static "))

    (if isPublic
	(setf f-string (concat f-string "public function "))
      (setf f-string (concat f-string "private function ")))

    (indent-according-to-mode)
    (insert f-string)

    (save-excursion
      (insert "(){")
      (insert "\n\n")
      (indent-according-to-mode)
      (insert "}"))))

(defun as-insert-flash-var()
  (interactive)
  (let ((isStatic (y-or-n-p "Static? "))
	(isPublic (y-or-n-p "Public? "))
	(f-string ""))

    (when isStatic
      (setf f-string "static "))

    (if isPublic
	(setf f-string (concat f-string "public "))
      (setf f-string (concat f-string "private ")))

    (indent-according-to-mode)
    (insert (concat f-string "var "))))

(defun as-get-classname(buffer)
  (let* ((wholePath (buffer-file-name buffer))
	 (filename (file-name-nondirectory wholePath))
	 (className (file-name-sans-extension filename)))
    className))

(defun toggle-build-source (source-folder build-folder)
  "If we are visiting a file in the source folder, open the corresponding file
in the build folder, and vice-versa."
  (interactive
   (list
    (get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
    (get-or-ask 'actionscript-mode 'build-folder '(read-file-name "Which is the build folder?" "src"))))

  ;; get the path to the current buffer
  (let ((wholePath buffer-file-name)
	(build-regexp (concat "^" (expand-file-name build-folder) "\\(.*\\)"))
	(source-regexp (concat "^" (expand-file-name source-folder) "\\(.*\\)")))

    ;; check if it is within the build or the source folder
    (if (string-match source-regexp wholePath)
					; We are in the source directory
	;;(message (concat "source: " (match-string 1 wholePath)))
	(find-file (concat build-folder (match-string 1 wholePath)))
      (if (string-match build-regexp wholePath)
					; we are in the build directory
	  (find-file (concat source-folder (match-string 1 wholePath)))
	(message "Not in build or source folder.")))))

(define-key actionscript-mode-map [f5] 'as-print-func-info)
;;Nobody messes with my character transpose function...
;;(define-key actionscript-mode-map "\C-t" 'as-insert-trace)
(define-key actionscript-mode-map [f4] 'as-flash-compile)
(define-key actionscript-mode-map "\C-c\C-v" 'as-insert-flash-var)
(define-key actionscript-mode-map "\C-c\C-f" 'as-insert-flash-func)

(when running-on-x
  (setq special-display-buffer-names
	'(("*as-build*" 
	   (background-color . "#000018")
	   (width . 120) 
	   (height . 30)
	   (auto-raise)))))
