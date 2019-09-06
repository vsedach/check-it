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

(in-package :check-it-test)

(in-root-suite)

(defsuite* deterministic-tests)

(deftest test-int-shrink ()
  (loop for int in (list 5 20 100 300)
     do
       (is (= (shrink int #'int-tester) 5))))

(deftest test-list-shrink ()
  (loop for size in (list 5 20 50 100)
       do
       (is (equal (shrink (make-list size :initial-element nil) #'list-tester)
                  '(nil nil nil nil nil)))))

(deftest test-struct-slot-names ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (is (equalp (check-it::struct-slot-names test-struct)
                (list 'a-slot 'another-slot)))))

#-(or abcl allegro)
(deftest test-copy-structure-and-slots ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (is (equalp (check-it::copy-structure-and-slots
                 test-struct
                 (list 'a-slot 'another-slot))
                test-struct))))

(deftest test-struct-shrink ()
  (let ((test-struct (make-a-struct :a-slot 5 :another-slot 5)))
    (loop for i in (list 5 20 100 300)
       for j in (list 5 20 100 300)
       do
         (is (equalp (shrink (make-a-struct
                              :a-slot i
                              :another-slot j)
                             #'struct-tester)
                     test-struct)))))

(deftest test-extract-params-from-lambda-list ()
  (let ((cases
         '(((a) (a))
           ((&optional a) (a))
           ((&rest a) (a))
           ((&key a) (a))
           ((&aux a) (a))
           ((a &optional b &rest c &key d &aux e) (a b c d e))
           ((&optional a (b) (c 1) (d 2 d-supplied)) (a b c d))
           ((&key a (b) (c 1) (d 2 c-supplied) ((:e e)) ((:f f) 3) ((:g g) 4 g-supplied))
            (a b c d e f g))
           ((&aux a (b 1)) (a b)))))
    (loop for case in cases
       do
         (progn
           (is (equal (check-it::extract-params-from-lambda-list
                       (first case))
                      (second case)))))))
