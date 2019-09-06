;;; Copyright 2015 Kyle Littler
;;; Copyright 2015 Sebastian Christ <rudolfo.christ@gmail.com>

;;; SPDX-License-Identifier: LGPL-3.0-or-later

;;; This file is part of check-it.

;;; check-it is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.

;;; check-it is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with check-it. If not, see
;;; <https://www.gnu.org/licenses/>.

(in-package :check-it)

(defmacro destructuring-lambda (params &body body)
  "Pass the arguments of a destructuring lambda list to a function body."
  (with-gensyms (shallow-params)
    `(lambda (&rest ,shallow-params)
       (destructuring-bind (,params) ,shallow-params
         ,@body))))

(defun extract-params-from-lambda-list (lambda-list)
  "Return a list of the names of all parameters in an ordinary lambda list."
  (multiple-value-bind (required optional rest keys allow-other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys keyp))
    (append required
            (mapcar #'first optional)
            (when rest (list rest))
            (mapcar #'cadar keys)
            (mapcar #'first aux))))

(defmacro with-obvious-accessors (accessors instance &body body)
  "Like WITH-ACCESSORS but with the brevity of WITH-SLOTS."
  `(with-accessors
         (,@(loop for accessor in accessors
               collect `(,accessor ,accessor)))
       ,instance
     ,@body))

(defun join-list (char-list)
  "Join the given CHAR-LIST of characters to a string. '(#\a #\b #\b) => \"abc\""
  (if (null char-list)
      ""
      (reduce (lambda (str char)
                   (concatenate 'string str (princ-to-string char)))
                 (rest char-list)
                 :initial-value (princ-to-string (first char-list)))))

(defun make-struct-from-type (type-name)
  #-(or abcl allegro)
  (make-instance type-name)
  #+(or abcl allegro)
  (with-input-from-string
      (s (format nil "(~A::~A)"
                 (package-name (symbol-package type-name))
                 (symbol-name type-name)))
    (funcall (get-dispatch-macro-character #\# #\S)
             s #\S nil)))
