;;; Copyright 2015 Kyle Littler

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

(defclass reified-error ()
  ((wrapped-error
    :initarg :wrapped-error
    :accessor wrapped-error)))

(defmacro regression-case (&key name datum timestamp)
  `(regression-case% ',name ,datum ,timestamp))

(defclass regression-case ()
  ((name
    :initarg :name
    :reader name)
   (datum
    :initarg :datum
    :reader datum)
   (timestamp
    :initarg :timestamp
    :reader timestamp)))

(defun regression-case% (name datum timestamp)
  (push
   (make-instance 'regression-case
                  :name name
                  ;; Some external representations can't be dumped to FASL
                  ;; files, so for now it's simplest to delay loading serialized
                  ;; forms until runtime
                  :datum (read-from-string datum)
                  :timestamp timestamp)
   (get name 'regression-cases)))

(defun write-regression-case (name data-string)
  "Produce code for a regression case to be written to a file."
  `(regression-case
    :name ,name
    :datum ,(format nil "~A" data-string)
    :timestamp ,(get-universal-time)))

(defparameter *check-it-output* *standard-output*)

(defparameter *package-regression-files* (make-hash-table))

(defun register-package-regression-file (package regression-file)
  "Register a file to be used for saving all regression cases for a package."
  (setf (gethash
         (find-package package)
         *package-regression-files*)
        regression-file))

(defgeneric errored (result)
  (:method (result) nil)
  (:method ((result reified-error)) t))

(defun wrap-test-for-error-reporting (test)
  "Return a function capturing unhandled errors in TEST as data."
  (lambda (arg)
    (handler-case
        (funcall test arg)
      (error (c)
        (make-instance 'reified-error :wrapped-error c)))))

(defun wrap-test-for-shrinking (test)
  "Return a function treating unhandled errors in TEST as failures."
  (lambda (arg)
    (handler-case
        (funcall test arg)
      (error () nil))))

(defun check-it% (test-form generator test
                  &key
                    examples
                    shrink-failures
                    (random-state t)
                    (regression-id nil regression-id-supplied)
                    (regression-file (when regression-id-supplied
                                       (gethash
                                        (symbol-package regression-id)
                                        *package-regression-files*))))
  (let ((error-reporting-test
         (wrap-test-for-error-reporting test))
        (shrink-test
         (wrap-test-for-shrinking test)))
    (block trial-run
      (loop for example in examples
         do
           (let ((result (funcall error-reporting-test example)))
             ;; to-do: DRY this up
             (cond
               ((null result)
                (format *check-it-output* "~&Test ~A failed on example arg ~A~%"
                        test-form
                        example)
                (return-from trial-run nil))
               ((errored result)
                (format *check-it-output* "~&Test ~A signaled error ~A on example arg ~A~%"
                        test-form
                        (wrapped-error result)
                        example)
                (return-from trial-run nil)))))
      (when regression-id
        (loop for regression-case in (get regression-id 'regression-cases)
           do
             (let* ((datum (datum regression-case))
                    (result (funcall error-reporting-test datum)))
               (cond
                 ((null result)
                  (format *check-it-output* "~&Test ~A failed regression ~A with arg ~A~%"
                          test-form
                          regression-id
                          datum)
                  (return-from trial-run nil))
                 ((errored result)
                  (format *check-it-output* "~&Test ~A signaled error ~A on regression ~A with arg ~A~%"
                          test-form
                          (wrapped-error result)
                          regression-id
                          datum)
                  (return-from trial-run nil))))))
      (let ((*random-state* (make-random-state random-state)))
        (loop repeat *num-trials*
           do
             (progn
               (generate generator)
               (let (;; produce readable representation before anybody mutates this value
                     (stringified-value (format nil "~S" (cached-value generator)))
                     (result (funcall error-reporting-test (cached-value generator))))
                 (flet ((save-regression (string)
                          (when regression-file
                            (push string (get regression-id 'regression-cases))
                            (with-open-file (s regression-file
                                               :direction :output
                                               :if-exists :append
                                               :if-does-not-exist :error)
                              (format s "~&~S~%"
                                      (write-regression-case regression-id string)))))
                        (do-shrink ()
                          (let ((shrunk (shrink generator shrink-test)))
                            (format *check-it-output* "~&Shrunken failure case:~%~A~%" shrunk)
                            shrunk)))
                   (cond
                     ((null result)
                      (format *check-it-output*
                              "~&Test ~A ~A failed with random state:~%~S~%with arg ~A~%"
                              test-form
                              test
                              *random-state*
                              stringified-value)
                      (save-regression (if shrink-failures
                                           (format nil "~S" (do-shrink))
                                           stringified-value))
                      (return-from trial-run nil))
                     ((errored result)
                      (format *check-it-output*
                              "~&Test ~A ~A signaled error ~A with random state:~%~S~%with arg ~A~%"
                              test-form
                              test
                              (wrapped-error result)
                              *random-state*
                              stringified-value)
                      (save-regression (if shrink-failures
                                           (format nil "~S" (do-shrink))
                                           stringified-value))
                      (return-from trial-run nil))))))))
      (return-from trial-run t))))

(defmacro check-it (generator test
                    &key
                      examples
                      (shrink-failures t)
                      (random-state t random-state-supplied)
                      (regression-id nil regression-id-supplied)
                      (regression-file nil regression-file-supplied))
  "Macro for performing a randomized test run."
  `(check-it% ',test ,generator ,test
              :examples ,examples
              :shrink-failures ,shrink-failures
              ,@(when random-state-supplied `(:random-state ,random-state))
              ,@(when regression-id-supplied `(:regression-id ',regression-id))
              ,@(when regression-file-supplied `(:regression-file ,regression-file))))
