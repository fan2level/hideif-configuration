;;; hideif-conf.el --- initialize defines from file
;; 
;; Author: SEONGBAEK KANG
;; Keywords: c, hideif
;; 
;; This is used to load defines from files using `hide-ifdef-mode'.
;; The default directory is `hide-ifdef-conf-root', 
;; 
;; make file ~/.emacs.d/hideif/project-name
;;   contents in the file as below
;;     ------------------------------------
;;     DEFINE_1
;;     DEFINE_2
;;     DEFINE_3
;;
;;     ------------------------------------
;; 
;; project is a plain file, the defines is seperated bye a new line.
;; 
;; and you can just do from .emacs
;; (load-file "/path-to/`hideif-conf.el'")
;; 
;; and M-x hide-ifdef-conf-load
;; 
;; -----------------------------------------------------------------------------
;; 2014.02.20, 
;; add feature files in the root directory
;; ~/.emacs.d/hideif/
;; ~/.emacs.d/hideif/plain-text
;; ~/.emacs.d/hideif/directory1
;; ~/.emacs.d/hideif/directory1/plain-text1
;; ~/.emacs.d/hideif/directory1/plain-text2
;; ~/.emacs.d/hideif/directory2
;; in the above, there is 3 project as `plain-text', `directory1' and `directory2'
;;   1. `plaint-text' is project-name and contents of the project is added 
;;       as defines of `plain-text'.
;;   2. `directory1' is project-name and contents of the project(plain-text1, plain-text2) 
;;       is added as defines of `directory1'
;; 

;; this utility use the variable `hide-ifdef-define-alist' in the `hideif.el'
(require 'hideif)

(defcustom hide-ifdef-conf-root "~/.emacs.d/hideif/"
  "root directory of hideif configuration"
  :type 'string
  :group 'hide-ifdef-conf)

(defcustom hide-ifdef-conf-project-test "test"
  "test project of hideif configuration")

(defun hide-ifdef-conf-load (&optional root)
  "initialize defines from files"
  (interactive)
  (let ((project)
	(project-list))
    (if (null root)
	(setq project  hide-ifdef-conf-root)
      (setq project root))
    (make-directory project t)

    ;; reset `hide-ifdef-define-alist'
    (setq hide-ifdef-define-alist nil)

    (dolist (elt (hide-ifdef-conf-list-project project) project-list)
      (hide-ifdef-conf-load-project (concat project elt))
      (add-to-list 'project-list elt))
    )
  )

(defun hide-ifdef-conf-list-project (root)
  "listup project list under root directory
project is files and directory"
  (let (project-list)
    (dolist (elt (directory-files root) project-list)
      (if (and
	   (null (string-equal elt hide-ifdef-conf-project-test))
	   (null (string-equal elt "."))
	   (null (string-equal elt ".."))
	   )
	  (add-to-list 'project-list elt)))
    ))

(defun hide-ifdef-conf-load-project (project)
  "load define from project which is file or directory
if project is directory, all files in the directory is loaded"

  (if (file-regular-p project)
      (hide-ifdef-conf-load-feature (intern (file-name-nondirectory project)) project)
    (dolist (elt (directory-files project t))
      (if (and
	   (null (string-equal (file-name-nondirectory elt) "."))
	   (null (string-equal (file-name-nondirectory elt) ".."))
	   )
	  (hide-ifdef-conf-load-feature (intern (file-name-nondirectory elt)) elt))
      )
    )
  )

(defun hide-ifdef-conf-load-feature (project feature)
  "load defines from files which includes defines"
  (let ((inputs)
	(defines-string)
	(project-defines))

    (setq inputs (with-temp-buffer
		   (insert-file-contents-literally feature)
		   (split-string (buffer-string) "\r\n" t)))

    (dolist (elt inputs defines-string)
      (add-to-list 'defines-string (split-string elt " " t)))

    (setq project-defines (assoc project hide-ifdef-define-alist))
    (when (null project-defines)
	(setq project-defines (cons project nil))
	(push project-defines hide-ifdef-define-alist))

    (let (define)
      (dolist (elt defines-string)
	(setq define (intern (car elt)))
	(if (null (cdr project-defines))
	    (setcdr project-defines (cons define nil))
	  (if (null (member define (cdr project-defines)))
	      (nconc (cdr project-defines) (cons define nil)))))
      )
    )
  )
