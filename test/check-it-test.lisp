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

(defun run-all-tests ()
  (format t "~&Running deterministic tests:~%")
  (deterministic-tests)
  (format t "~&Running randomized tests:~%")
  (randomized-tests))

(defun int-tester (int)
  (< int 5))

(defun list-tester (list)
  (< (length list) 5))

(defun struct-tester (struct)
  (or (< (a-struct-a-slot struct) 5)
      (< (a-struct-another-slot struct) 5)))

(defun tuple-tester (tuple)
  (some (lambda (x) (< (abs x) 5)) tuple))

(defun greater-than-5 (num)
  (> (abs num) 5))

(defstruct a-struct
  a-slot
  another-slot)
