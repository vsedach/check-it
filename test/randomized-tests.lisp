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

(in-package :check-it-test)

(register-package-regression-file
 :check-it-test
 (merge-pathnames "test/regression-cases.lisp" *system-directory*))

(in-root-suite)

(defsuite* randomized-tests)

(deftest test-generator ()
  (is (every #'identity
             (mapcar (lambda (x y) (subtypep (type-of x) y))
                     (generate (generator (tuple (real) (integer) (list (integer)))))
                     '(single-float integer
                       #-abcl (or cons null)
                       #+abcl t ;; ridiculous
                       )))))

(deftest test-int-generate ()
  (loop for size in (list 10 20 50 300)
     do
       (let ((*size* size))
         (let ((g (generator (integer * 2))))
           (loop repeat 100
              do
                (is (<= (- *size*) (generate g) 2))))
         (let ((g (generator (integer -2 *))))
           (loop repeat 100
              do
                (is (<= -2 (generate g) *size*))))
         (let ((g (generator (integer -2 2))))
           (loop repeat 100
              do
                (is (<= -2 (generate g) 2))))
         (let ((g (generator (integer))))
           (loop repeat 100
              do
                (is (<= (- *size*) (generate g) *size*)))))))

(deftest test-real-generate ()
  (loop for size in (list 10 20 50 300)
     do
       (let ((*size* size))
         (let ((g (generator (real * 2))))
           (loop repeat 100
              do
                (is (<= (- *size*) (generate g) 2))))
         (let ((g (generator (real -2 *))))
           (loop repeat 100
              do
                (is (<= -2 (generate g) *size*))))
         (let ((g (generator (real -2 2))))
           (loop repeat 100
              do
                (is (<= -2 (generate g) 2))))
         (let ((g (generator (real))))
           (loop repeat 100
              do
                (is (<= (- *size*) (generate g) *size*)))))))

(deftest test-char-generate ()
  (loop for params in '((* *) (* 25) (25 *) (25 26))
     do (let ((g (generator (character (first params) (second params)))))
          (loop repeat 100
             do
               (is (<= (or (and (eq '* (first params)) 0) (first params))
                       (char-code (generate g))
                       (or (and (eq '* (second params)) 127) (second params))))))))

(deftest test-alphanumeric-generate ()
  (loop for g in (list (generator (alpha))
                       (generator (alphanumeric)))
     do
       (loop repeat 100
          do
            (let ((random-char (generate g)))
              (is (or (<= 48 (char-code random-char) 57)
                      (<= 65 (char-code random-char) 90)
                      (<= 97 (char-code random-char) 122)))))))

;;;; Shrink results of generators

(deftest test-int-generate-shrink ()
  (let ((g (generator (guard #'positive-integer-p (integer)))))
    (loop repeat 100
       do
         (is (= (shrink-and-trap-errors (generate g) (constantly nil)) 0))))
  (let ((g (generator (integer 1))))
    (loop repeat 100
       do
         (is (= (shrink-and-trap-errors (generate g) (constantly nil)) 0)))))

(deftest test-struct-generate-shrink ()
  (let ((g (generator (struct a-struct
                              :a-slot (integer)
                              :another-slot (integer))))
        (test-struct (make-a-struct :a-slot 0 :another-slot 0)))
    (loop repeat 100
       do
         (is (equalp (shrink-and-trap-errors (generate g) (constantly nil))
                     test-struct)))))

;;;; Shrink generators themselves

(deftest test-shrink-error ()
  ;; errors should be caught and treated as test failures in shrinking
  (let ((g (generator (integer))))
    (loop repeat 30
       do
         (progn
           (generate g)
           (is (= (shrink-and-trap-errors
                   g
                   (lambda (x) (declare (ignore x)) (error "barf")))
                  0))))))

(deftest test-int-generator-shrink ()
  (let ((*size* 30))
    (let ((g (generator (integer 5))))
      (loop repeat 100
         do
           (progn
             (generate g)
             (is (= (shrink-and-trap-errors g (lambda (x) (< x 3))) 5))
             (loop for try = (generate g)
                until (>= (cached-value g) 9))
             (is (= (shrink-and-trap-errors g (lambda (x) (< x 9))) 9)))))
    (let ((g (generator (integer * 8))))
      (loop repeat 100
         do
           (progn
             (generate g)
             (is (= (shrink-and-trap-errors g (lambda (x) (> x 10))) 0))
             (loop for try = (generate g)
                until (<= (cached-value g) 3))
             (is (= (shrink-and-trap-errors g (lambda (x) (> x 3))) 0))
             (loop for try = (generate g)
                until (<= (cached-value g) -3))
             (is (= (shrink-and-trap-errors g (lambda (x) (> x -3))) -3)))))
    (let ((g (generator (integer 5 9))))
      (loop repeat 100
         do
           (progn
             (generate g)
             (is (= (shrink-and-trap-errors g (lambda (x) (< x 3))) 5))
             (loop for try = (generate g)
                until (>= (cached-value g) 6))
             (is (= (shrink-and-trap-errors g (lambda (x) (< x 6))) 6)))))))

(deftest test-tuple-generator-shrink ()
  (let ((g (generator (tuple (integer) (integer) (integer)))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (is (equal (shrink-and-trap-errors g (constantly nil))
                      (list 0 0 0))))))
  (let ((g (generator (tuple
                       (guard #'greater-than-5 (integer))
                       (guard #'greater-than-5 (integer))
                       (guard #'greater-than-5 (integer))))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (is (every (lambda (x) (= (abs x) 6))
                      (shrink-and-trap-errors g #'tuple-tester))))))
  (let ((g (generator (tuple (integer 6)
                             (integer 6)
                             (integer 6)))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (is (every (lambda (x) (= (abs x) 6))
                      (shrink-and-trap-errors g #'tuple-tester)))))))

(deftest test-list-generator-bounds ()
  (let ((min-g (generator (integer 0)))
        (interval-g (generator (integer 1))))
    (loop repeat 20
       do
         (progn
           (let* ((min (generate min-g))
                  (interval (generate interval-g))
                  (list-g
                   (generator (list (integer)
                                    :min-length min
                                    :max-length (+ min interval)))))
             (is (<= min (length (generate list-g)) (+ min interval)))))))
  (let ((bound-g (generator (integer 0))))
    (loop repeat 10
       do
         (progn
           (let* ((bound (generate bound-g))
                  (list-g
                   (generator (list (integer)
                                    :length bound))))
             (is (= bound (length (generate list-g)))))))))

(deftest test-list-generator-shrink ()
  (let ((g (generator
            (guard (lambda (l) (> (length l) 5))
                   (list
                    (guard #'greater-than-5
                           (integer)))))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (shrink-and-trap-errors g #'list-tester)
           (is (and (= (length (cached-value g)) 6)
                    (every (lambda (x) (= (abs x) 6)) (cached-value g))))))))

(deftest test-char-generator-shrink ()
  (let ((g (generator (character #\c #\q))))
    (is (eql (generate g)
             (shrink g (constantly nil))))))

(deftest test-string-generator-shrink ()
  (let ((g (generator (guard (lambda (s) (> (length s) 5))
                             (string)))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (shrink-and-trap-errors g (lambda (s) (> 5 (length s))))
           (is (= (length (cached-value g)) 6))))))

(deftest test-struct-generator-shrink ()
  (let ((g (generator (struct a-struct
                              :a-slot (guard #'greater-than-5 (integer))
                              :another-slot (guard #'greater-than-5 (integer))))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (shrink-and-trap-errors g (lambda (x)
                       (or (< (abs (a-struct-a-slot x)) 5)
                           (< (abs (a-struct-another-slot x)) 5))))
           (is (and (= (abs (a-struct-a-slot (cached-value g))) 6)
                    (= (abs (a-struct-another-slot (cached-value g))) 6)))))))

(deftest test-or-generator-shrink ()
  ;; ensure or-generator won't hop to a nonconstant alternative
  (let ((*size* 25)
        (g (generator (or
                       (integer 15 20)
                       (integer 5 10)))))
    (loop repeat 100
       do
         (progn
           (loop for try = (generate g)
              until (>= (cached-value g) 15))
           (is (= (shrink-and-trap-errors g (constantly nil)) 15))))))

(def-generator derp ()
  (generator (or (integer) (derp))))

(deftest test-custom-generator ()
  (let ((g (generator (derp)))
        (*size* 10))
    (loop repeat 20
       do
         (is (<= -10 (generate g) 10)))))

(def-generator herp ()
  (generator (or (integer 10) (herp))))

(deftest test-custom-generator-shrink ()
  (let ((g (generator (herp)))
        (*size* 20))
    (loop repeat 20
       do
         (progn
           (generate g)
           (is (= (shrink-and-trap-errors g #'int-tester) 10))))))

(deftest test-check-it ()
  (let ((*num-trials* 50)
        (*check-it-output* nil))
    (is (check-it (generator (integer))
                  (lambda (x) (<= x *size*))
                  :examples (list *size*)))))

(define-condition test-error (error)
  ())

(deftest test-check-it-should-error ()
  (let ((*check-it-output* nil)
        (g (generator (integer))))
    (is (check-it g
                  (lambda (x)
                    (declare (ignore x))
                    (handler-case
                        (progn
                          (error 'test-error)
                          nil)
                      (test-error ()
                        t)))))))

(deftest test-chained-generator ()
  (let ((*list-size* 100)
        (*size* 100)
        (g
         (generator
          (chain ((x (integer 10 12))
                  (y (integer 18 20)))
            (generator (list (integer) :min-length x :max-length y))))))
    (let ((lengths
           (loop repeat 50
              collect (length (generate g)))))
      (is (<= 10 (apply #'min lengths) (apply #'max lengths) 20)))
    (loop repeat 20
       do
         (progn
           (generate g)
           (is (<= 10 (length (shrink-and-trap-errors g (lambda (x) (= (length x) 5)))) 12))))
    (let ((*check-it-output* nil))
      (is
       (check-it g
                 (lambda (x) (<= 10 (length x) 20))
                 :examples (list (iota 10) (iota 20))))))
  (let ((*list-size* 100)
        (*size* 100)
        (g (generator
            (chain ((x (integer 10 20))
                    (y (integer 30 40)))
              (generator (list (integer x y) :min-length x :max-length y))))))
    (loop repeat 50
       do
         (progn
           (generate g)
           (let ((min (apply #'min (mapcar #'cached-value
                                           (check-it::sub-generators
                                            (cached-generator g)))))
                 (max (apply #'max (mapcar #'cached-value
                                           (check-it::sub-generators
                                            (cached-generator g))))))
             (is (<= (check-it::min-length (cached-generator g))
                     min
                     max
                     (check-it::max-length (cached-generator g))))
             (shrink g (constantly nil))
             (let ((min (apply #'min (mapcar #'cached-value
                                             (check-it::sub-generators
                                              (cached-generator g)))))
                   (max (apply #'max (mapcar #'cached-value
                                             (check-it::sub-generators
                                              (cached-generator g))))))
               (is (= (check-it::min-length (cached-generator g))
                      min
                      max)))))))
  (let ((*list-size* 100)
        (*size* 100)
        (g (generator
            (list
             (chain ((x (integer 10 20))
                     (y (integer 21 30)))
               (generator (list (integer x y) :min-length x :max-length y)))
             :min-length 5 :max-length 10))))
    (loop repeat 50
       do
         (progn
           (generate g)
           (shrink g (constantly nil))
           (is
            (and (= (length (cached-value g)) 5)
                 (every (lambda (x)
                          (mapcar #'= (list* (length x) x)))
                        (cached-value g))))))))

(deftest test-mapped-generator-shrink ()
  (let ((*size* 100)
        (g (generator (tuple (map (lambda (x) (list x x x)) (integer 3 50))
                             (map (lambda (x) (list x x)) (integer 3 50))))))
    (loop repeat 10
         do
         (progn
           (generate g)
           (is (equal (shrink-and-trap-errors g (constantly nil)) '((3 3 3) (3 3))))))))

(deftest test-mapped-generator-multiarg-shrink ()
  (let ((*size* 100)
        (g (generator (map (lambda (x y) (list x (+ x y))) (integer 3 50) (integer 4 50)))))
    (loop repeat 10
       do
         (progn
           (generate g)
           (is (equal (shrink-and-trap-errors g (constantly nil))
                      (list 3 7)))))))

(deftest test-int-generator-regenerate ()
  (let ((g (generator (integer))))
    (loop repeat 10
       do
         (is (= (generate g) (regenerate g))))))

(deftest test-real-generator-regenerate ()
  (let ((g (generator (real))))
    (loop repeat 10
       do
         (is (= (generate g) (regenerate g))))))

(deftest test-char-generator-regenerate ()
  (let ((g (generator (character))))
    (loop repeat 10
       do
         (is (eql (generate g) (regenerate g))))))

(deftest test-tuple-generator-regenerate ()
  (let ((g (generator (tuple (integer) (integer) (integer)))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-list-generator-regenerate ()
  (let ((g (generator (list (integer)))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-string-generator-regenerate ()
  (let ((g (generator (string))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-struct-generator-regenerate ()
  (let ((g (generator (struct a-struct
                              :a-slot (integer)
                              :another-slot (real)))))
    (loop repeat 10
       do
         (is (equalp (generate g) (regenerate g))))))

(deftest test-mapped-generator-regenerate ()
  (let ((g (generator (map (lambda (x) (list x x x)) (list (integer))))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-mapped-generator-multiarg-regenerate ()
  (let ((g (generator (map (lambda (x y) (list x x y)) (integer) (tuple (integer))))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-chained-generator-regenerate ()
  (let ((g (generator
            (chain ((x (integer 10 20))
                    (y (integer 21 30)))
              (generator (list (integer x y) :min-length x :max-length y))))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-guard-generator-regenerate ()
  (let ((g (generator
            (guard (lambda (x) (> (length x) 1)) (list (integer))))))
    (loop repeat 10
       do
         (is (equal (generate g) (regenerate g))))))

(deftest test-or-generator-regenerate ()
  (let ((g (generator
            (or (list (integer))
                (tuple (integer) (real))
                (struct a-struct
                        :a-slot (integer)
                        :another-slot (real))))))
    (loop repeat 10
       do
         (is (equalp (generate g) (regenerate g))))))

(def-generator big-custom-generator ()
  (generator
   (or (list (integer) :min-length 2)
       (tuple (integer) (real))
       (struct a-struct
               :a-slot (integer)
               :another-slot (real)))))

(deftest test-custom-generator-regenerate ()
  (let ((g (generator (big-custom-generator))))
    (loop repeat 10
       do
         (is (equalp (generate g) (regenerate g))))))
