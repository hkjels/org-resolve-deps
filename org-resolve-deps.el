;; Org resolve deps

;; This package will walk all the include-statements of an org-mode file
;; and temporarily replace them with the content of the files specified
;; at tangling.

;; Copyright (C) 2020  Henrik Kjerringvåg
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'org)

(defgroup org-resolve-deps nil
  "Resolve include-statements before tangling"
  :prefix "org-resolve-deps-"
  :group 'org
  :link '(url-link :tag "Github" "https://github.com/hkjels/org-resolve-deps"))

;; Do you prefer ~org-resolve-deps~ over the original tangle functions?

;; By default, you will have to explicitly use org-resolve-deps tanling
;; functions to tangle a project, but by setting ~org-resolve-deps-advice~
;; to ~t~, the original ~org-babel~ tangling functions will be enhanced.

(defcustom org-resolve-deps-advice nil
  "Should include-statements be resolved when using the original
   org-babel tangling functions?"
  :type '(boolean)
  :group 'org-resolve-deps)

(defvar org-resolve-deps--tangle-cmd nil)



;; We create a temporary file named ~.org-resolve-deps.org~ to be able to
;; tangle. This file can automatically be deleted after tangling, by
;; setting ~org-resolve-deps-delete-temp-file~ to ~t~. Note that this can
;; break whatever ~after-save~ hooks you might have, so it's advised to
;; keep it's value negative.

(defcustom org-resolve-deps-delete-temp-file nil
  "Delete the temporary file made for tangling"
  :type '(boolean)
  :group 'org-resolve-deps)

;; Resolve dependencies & tangle

;; We need to check the ~:tangle~ argument and make it adhere to how we
;; resolve dependencies.

(defun org-resolve-deps-header-tangle (str root file)
  "Ensure that the tangle argument uses the path specified
If `:tangle` is set to `yes` it will use the current file as base.
`org-resolve-deps` introduces a new `:tangle` option `root`.
Specifying `root` will tangle using the root-file as base. Using a
file-path as argument, works as you'd expect; with relative path being
relative to the file it's in."
  (replace-regexp-in-string
   ":tangle \"?\\([^\n:\"]+\\)\"?"
   (lambda (tangle)
     (cond ((string= "yes" tangle) (file-name-sans-extension file))
           ((string= "root" tangle) (file-name-sans-extension root))
           ((string= "no" tangle) "no")
           ((> (length tangle) 0)
            (save-match-data
              (let ((out (pop (cdr (split-string (concat "" tangle))))))
                (if (or (string= "(" (substring out 0 1))
                        (file-name-absolute-p out))
                    out
                  (expand-file-name out (file-name-directory file))))))))
   str nil 'literal 1))



;; This is where the bulk of the heavy lifting happens. We walk the
;; dependencies ("include statements") and replace them with the content
;; of the files they point to. We also keep track of what files have been
;; included to not end up with cyclic dependencies.

(defun org-resolve-deps-of-file (root &optional file start files-included)
  "Recursively replace include-statements with the targeted content"
  (let* ((file (or file root))
         (basedir (file-name-directory file))
         (pattern "^#\\+include:\s*\"\\([^\"]+\\)\"[^\n]*$")
         (start (or start (point-min)))
         (files-included (or files-included '())))
    (if (member file files-included)
        (user-error "org-resolve-deps: Found a cyclic dependency `%s`" file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char start)
        (while (re-search-forward pattern nil 'noerror)
          (let* ((include-file (match-string 1))
                 (path (expand-file-name include-file basedir))
                 (new-start (match-end 0))
                 (files-included (cons file files-included))
                 (tree (org-resolve-deps-of-file root path new-start files-included)))
            (replace-match (org-resolve-deps-header-tangle tree root path) nil 'literal)))
        (insert "\n")
        (buffer-string)))))



;; You call ~org-resolve-deps-tangle-file~ or ~org-resolve-deps-tangle~ just
;; as you would call the original ~org-babel-tangle~ functions. If you
;; supply any arguments, the call is just relayed to the original
;; functions as ~org-resolve-deps~ only work on a per project level.

(defun org-resolve-deps-tangle-file (file &optional target-file lang)
  "Tangle the specified file and all of it's includes"
  (interactive "P")
  (if (and (equal nil target-file)
           (equal nil lang))
      (if (file-exists-p file)
          (let* ((buf (generate-new-buffer "*org-resolve-deps*"))
                 (basedir (file-name-directory file))
                 (tmp-file (expand-file-name ".org-resolve-deps.org" basedir))
                 (revert-without-query (list "org-resolve-deps.org")))
            (with-current-buffer buf
              (insert (org-resolve-deps-of-file file))
              (write-file tmp-file))
            (set-buffer buf)
            (org-babel-tangle)
            (kill-buffer buf)
            (when org-resolve-deps-delete-temp-file
             (delete-file tmp-file)))
        (user-error "`org-resolve-deps`: Only files saved to disk can be tangled"))
    (progn (message "`org-resolve-deps`: Falling back to using original `org-babel-tangle-file` function")
           (org-babel-tangle-file file target-file lang))))

(defun org-resolve-deps-tangle (&optional arg target-file lang)
  "Tangle the current buffer and all of it's includes"
  (interactive "P")
  (if (equal nil arg)
      (org-resolve-deps-tangle-file (buffer-file-name) target-file lang)
      (progn (message "`org-resolve-deps`: Falling back to using original `org-babel-tangle` function")
             (org-babel-tangle arg target-file lang))))



;; Here we choose which functions to apply based on the value of ~org-resolve-deps-advice~.

(define-minor-mode org-resolve-deps-mode
  "Resolve includes before tangling"
  nil
  :lighter ""
  :global t
  :require 'org-resolve-deps
  (when org-resolve-deps-mode
    (advice-add 'org-babel-tangle :around
                (lambda (cmd &rest args)
                  (progn (setq org-resolve-deps--tangle-cmd cmd)
                         (if org-resolve-deps-advice
                             (apply #'org-resolve-deps-tangle args)
                           (apply cmd args)))))
    (advice-add 'org-babel-tangle-file :around
                (lambda (cmd &rest args)
                  (progn (setq org-resolve-deps--tangle-cmd cmd)
                         (if org-resolve-deps-advice
                             (apply #'org-resolve-deps-tangle-file args)
                           (apply cmd args)))))))

(provide 'org-resolve-deps)
