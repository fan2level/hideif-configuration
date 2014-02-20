;;; hideif-conf.el --- initialize defines from file
;; 
;; Author: SEONGBAEK KANG
;; Keywords: c, hideif
;; 
;; This is used to load defines from files using `hide-ifdef-mode'.
;; The default directory is `hide-ifdef-conf-root', 
;; 
;; ~/.emacs.d/hideif/test-project
;; DEFINE_1
;; DEFINE_2
;; DEFINE_3
;; 
;; test-project is a plain file, the defines is seperated bye a new line.
;; and you can just do from .emacs
;; (hide-ifdef-conf-load)
;; 

(require 'hideif)

(defcustom hide-ifdef-conf-root "~/.emacs.d/hideif/"
  "root directory of hideif configuration"
  :type 'string
  :group 'hide-ifdef-conf)

(defcustom hide-ifdef-conf-project-test "test"
  "test project of hideif configuration")

(defun hide-ifdef-conf-load (&optional root)
  "initialize ifdef from files"
  
  (let (projects)
    (if (null root)
	(setq root hide-ifdef-conf-root))
    (make-directory root t)

    (dolist (elt (hide-ifdef-conf-list-project root))
      (hide-ifdef-conf-load-project (concat root elt))
      (add-to-list 'projects elt))
    projects)
  )

(defun hide-ifdef-conf-list-project (&optional root)
  "listup project list under root directory
return the `list' of project"
  (let (project)
    (dolist (elt (directory-files-and-attributes root) project)
      (if (and 
	   (null (string-equal hide-ifdef-conf-project-test (car elt)))
	   (null (car (cdr elt)))
	   )
	(add-to-list 'project (car elt))))
    project)
  )
;; (hide-ifdef-conf-list-project hide-ifdef-conf-root)

(defun hide-ifdef-conf-load-project (project)
  "load ifdef from project which is filepath"

  ;; todo : check file is exist
  (let ((inputs (with-temp-buffer
		    (insert-file-contents-literally project)
		    (setq inputs (split-string (buffer-string) "\r\n" t)))
		    )
	(result)
	(model)
	(feature))

    (dolist (elt inputs result)
      (push (split-string elt " " t) result)
      )

    ;; initialize `hide-ifdef-define-alist'
    (if (assoc (intern (file-name-nondirectory project)) hide-ifdef-define-alist)
	(message "%s project is exist" (file-name-nondirectory project))

      (setq model (cons (intern (file-name-nondirectory project)) nil))
      (setcdr model (dolist (elt result feature)
		      (add-to-list 'feature (intern (car elt)) t)))
      (push model hide-ifdef-define-alist))
    model)
  )
;; (hide-ifdef-conf-load-project (concat hide-ifdef-conf-root hide-ifdef-conf-project-test))
;; (setq hide-ifdef-define-alist nil)
