;;;; Simple FIFO for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;


;;; ----------------------------------------------------

(defstruct (queue (:constructor %make-queue (xs &aux (ys (last xs)))))
  (head xs :read-only t)
  (tail ys :read-only t))

;;; ----------------------------------------------------

(defun make-queue (&key initial-contents)
  "Create a new queue."
  (%make-queue (cons nil initial-contents)))

;;; ----------------------------------------------------

(defmethod print-object ((q queue) stream)
  "Output a deque to a stream."
  (print-unreadable-object (q stream :type t)
    (format stream "~:[EMPTY~;~:*~a~]" (rest (queue-head q)))))

;;; ----------------------------------------------------

(defun queue-push (x q)
  "Push a value onto the tail of the queue."
  (with-slots (tail)
      q
    (car (setf tail (cdr (rplacd tail (list x)))))))

;;; ----------------------------------------------------

(defun queue-pop (q)
  "Pop a value off a queue, return the value and success flag."
  (with-slots (head)
      q
    (when (rest head)
      (values (car (setf head (cdr head))) t))))

;;; ----------------------------------------------------

(defun queue-list (q)
  "Return the list of elements in the queue; does not alter the queue."
  (rest (queue-head q)))
