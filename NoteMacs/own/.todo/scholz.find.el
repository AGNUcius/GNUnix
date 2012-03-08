;;http://lists.gnu.org/archive/html/help-gnu-emacs/2004-11/msg00110.html

(defun addinfo-subdirs (dir)
  "Return a list of direct child directories of directory DIR."
  (if (not (file-directory-p dir))
      (error "Not a directory: %s" dir)
    (cddr ; Get rid of "." and ".."
     (delq nil (mapcar (lambda (f)
                         (and (file-directory-p f)
                              f))
                       (directory-files dir t))))))

(defmacro do-addinfo-dir-tree (spec &rest body)
  "Run BODY once for each directory below DIRECTORY.
This traverses the directory tree below DIRECTORY (including)
depth first, binding VARIABLE to each directory name in turn.

\(fn (VARIABLE DIRECTORY) BODY)"
  (let ((var (car spec))
        (start-dir (cadr spec))
        (dir (make-symbol "--dir"))
        (subdirs (make-symbol "--subdirs"))
        (dir-queue (make-symbol "--dirqueue")))
    `(let ((,dir ,start-dir)
           ,subdirs ,dir-queue ,var)
       (unless (file-directory-p ,dir)
         (error "Not a directory: %s" ,dir))
       (while ,dir
         (setq ,subdirs (addinfo-subdirs ,dir)
               ,var ,dir)
         ,@body
         (cond (,subdirs
                (setq ,dir-queue (nconc (cdr ,subdirs)
                                        ,dir-queue)
                      ,dir (car ,subdirs)))
               (,dir-queue
                (setq ,dir (car ,dir-queue)
                      ,dir-queue (cdr ,dir-queue)))
               (t (setq ,dir nil)))))))


Example: to find all files with the extension ".info" below
$HOME, you could use it like this:

(let ((info-files nil))
  (do-addinfo-dir-tree (dir "~")
    (let ((files (delq nil
                       (mapcar (lambda (el)
                                 (and (not (file-directory-p el))
                                      el))
                               (directory-files dir)))))
      (dolist (f files)
        (when (string-match "\\.info\\'" f)
          (push f info-files)))))
  (nreverse info-files))

;;     Oliver
;; -- 
;; 16 Brumaire an 213 de la R√volution
;; Libert√, Egalit√, Fraternit√!
