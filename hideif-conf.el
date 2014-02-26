;;; hideif-conf.el --- initialize defines from file using `hideif'

;; Copyright (C) 2014 SEONGBAEK KANG

;; Author: SEONGBAEK KANG <asinwolf@gmail.com>
;; Maintainer:
;; Created: 20 Feb 2014
;; Keywords: c, `hideif'
;; Package-Version:
;; Package-Requires: ((hideif "24.3.1"))

;; This file is not part of GNU Emacs.

;; see <http://www.gnu.org/licenses/>

;;; Commentary:

;; To load definitions from files to `hide-ifdef-define-alist'
;; 
;; make project file or directory
;; if project is directory, all files of the directory is included.
;; 
;; "~/.emacs.d/hideif/" which is default root directory
;; 
;; you can use `hide-ifdef-conf-root' which is user defined variable.
;; default is nil.
;; if you want to use,
;; (setq 'hide-ifdef-conf-root "path-to-you-want")
;; 
;; it may be as below, project-file is a project name
;; 
;; "~/.emacs.d/hideif/`project-file'"
;; 
;; or you can place multiple files as one project
;; 
;; "~/.emacs.d/hideif/`project1'/device1-file"
;; "~/.emacs.d/hideif/`project1'/device2-file"
;; "~/.emacs.d/hideif/`project1'/device3-file"
;; 
;; for example of file' contents is 
;;   FEATURE_1
;;   FEATURE_2  34
;;   FEATURE_3
;; 
;; add to your init file(.emacs)
;;     (add-to-list 'load-path "path")
;;     (require 'hideif-conf)
;; 
;; to load defines
;; 
;;     (hide-ifdef-conf-load) or M-x hide-ifdef-conf-load

;;; History:
;; 2014.02.26
;;           documents
;;           user can add root direcotry as `hide-ifdef-conf-root'
;; 2014.02.25
;;           support defines's value.
;; 2014.02.21
;;           can load files from directory which is used `project' name
;; 2014.02.20 
;;           created

;;; Code:

(require 'hideif)

(defcustom hide-ifdef-conf-root-default "~/.emacs.d/hideif/"
  "root directory of hideif configuration - default"
  :type 'string
  :group 'hide-ifdef-conf)

(defvar hide-ifdef-conf-root nil
  "root directory of hideif configuration")

(defcustom hide-ifdef-conf-project-test "test"
  "test project of hideif configuration")

(defun hide-ifdef-conf-load (&optional root)
  "initialize defines from files
check root directory
1. parameter `root'
2. user define `hide-ifdef-conf-root' 
3. default `hide-ifdef-conf-root-default'
"
  (interactive)
  (let ((project)
	(project-list))
    (setq project (or root hide-ifdef-conf-root hide-ifdef-conf-root-default))
    (make-directory project t)

    ;; reset `hide-ifdef-define-alist' to nil
    (setq hide-ifdef-define-alist nil)

    (dolist (elt (hide-ifdef-conf-list-project project) project-list)
      (hide-ifdef-conf-load-project (concat project elt))
      (add-to-list 'project-list elt))
    )
  )

(defun hide-ifdef-conf-list-project (root)
  "listup project list under root directory
project can be files and directory under root"
  (let (project-list)
    (dolist (elt (directory-files root nil "^[^.]+") project-list)
      (if (and
	   (null (string-equal elt hide-ifdef-conf-project-test))
	   )
	  (add-to-list 'project-list elt)))
    ))

(defun hide-ifdef-conf-load-project (project)
  "load defines from project which is file or directory
if project is directory, all files in the directory is loaded"

  (if (file-regular-p project)
      (hide-ifdef-conf-load-feature (intern (file-name-nondirectory project)) project)
    (dolist (elt (directory-files project t "^[^.]+"))
      (hide-ifdef-conf-load-feature (intern (file-name-nondirectory project)) elt)
      )
    )
  )

(defun hide-ifdef-conf-load-feature (project feature)
  "load defines from file which includes defines"
  (let ((inputs)
	(defines-string)
	(project-defines))

    (setq inputs (with-temp-buffer
		   (insert-file-contents-literally feature)
		   (split-string (buffer-string) "[\r\n]+" t)))

    (dolist (elt inputs defines-string)
      (add-to-list 'defines-string (split-string elt "[ \t]" t)))

    (setq project-defines (assoc project hide-ifdef-define-alist))
    (when (null project-defines)
	(setq project-defines (cons project nil))
	(push project-defines hide-ifdef-define-alist))

    (let ((define nil)
	  (value nil))
      (dolist (elt defines-string)
	(setq define (intern (car elt)))
	(setq value (car (cdr elt)))
	(if (null (cdr project-defines))
	    (setcdr project-defines (if value
					;; fixme: value must be number
					(list (cons define (string-to-number value)))
					(cons define nil)) )
	  (if (null (member define (cdr project-defines)))
	      (nconc (cdr project-defines) (if value
					       ;; fixme: value must be number
					       (list (cons define (string-to-number value)))
					       (cons define nil))))))
      )
    project-defines)
  )

(defun hide-ifdef-conf-use-define-alist (name)
  "Wrapper function of `hide-ifdef-use-define-alist' in `hideif.el'
this can process define's value"
  (interactive
   (list (completing-read "Use define list:- "
                          (mapcar (lambda (x) (symbol-name (car x)))
                                  hide-ifdef-define-alist)
                          nil t)))
  (if (stringp name) (setq name (intern name)))
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
        (setq hide-ifdef-env
              (mapcar (lambda (arg) 
			;; (cons arg t)
			(if (consp arg)
			    (cons (car arg) (cdr arg))
			  (cons arg t))
			)
                      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (hide-ifdefs))))
(defalias 'hide-ifdef-use-define-alist 'hide-ifdef-conf-use-define-alist)

(provide 'hideif-conf)

;;; hideif-conf.el ends here
