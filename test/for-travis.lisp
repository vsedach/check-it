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

(defun run-tests-for-travis ()
  (let ((*debugger-hook*
         (lambda (c h)
           (declare (ignore h))
           (format t "~&~A~%" c)
           (force-output t)
           (sleep 1)
           (uiop:quit -1))))
    (handler-case
        (run-all-tests)
      ;; some Lisps appear not to use the condition's :report value,
      ;; so we reimplement it here explicitly
      (stefil::assertion-failed (c)
        (force-output t)
        (format t "~&Test assertion failed:~%~%")
        (describe (stefil::failure-description-of c) t)
        (format t "~%")
        (force-output t)
        (sleep 1)
        (uiop:quit -1)))))
