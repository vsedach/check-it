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

(defgeneric regenerate (generator)
  (:documentation
   "Regenerate a value equivalent to the previous value created with GENERATE."))

(defmethod regenerate (generator)
  "Treat non-generators as constants."
  generator)

(defmethod regenerate :around ((generator generator))
  (setf (cached-value generator) (call-next-method)))

(defmethod regenerate ((generator list-generator))
  (mapcar #'regenerate (sub-generators generator)))

(defmethod regenerate ((generator tuple-generator))
  (mapcar #'regenerate (sub-generators generator)))

(defmethod regenerate ((generator or-generator))
  (regenerate (cached-generator generator)))

(defmethod regenerate ((generator int-generator))
  (cached-value generator))

(defmethod regenerate ((generator real-generator))
  (cached-value generator))

(defmethod regenerate ((generator char-generator))
  (cached-value generator))

(defmethod regenerate ((generator string-generator))
  (let ((chars (call-next-method)))
    (setf (cached-str-list generator) chars)
    (join-list chars)))

(defmethod regenerate ((generator struct-generator))
  (let ((struct (make-struct-from-type (struct-type generator))))
    (loop for name in (slot-names generator)
       for gen in (slot-generators generator)
       do (setf (slot-value struct name)
                (regenerate gen)))
    struct))

(defmethod regenerate ((generator mapped-generator))
  (with-obvious-accessors (sub-generators mapping) generator
    (apply mapping (mapcar #'regenerate sub-generators))))

(defmethod regenerate ((generator chained-generator))
  (regenerate (cached-generator generator)))

(defmethod regenerate ((generator guard-generator))
  (regenerate (sub-generator generator)))

(defmethod regenerate ((generator custom-generator))
  (regenerate (sub-generator generator)))
