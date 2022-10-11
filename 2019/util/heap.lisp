;;;; Simple Binary Heap for Common Lisp
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


(defstruct (heap (:constructor %make-heap (test &key key)))

  ;; compare function using test and key again two heap elements
  (compare (if (null key)
             test
             (lambda (a b)
               (funcall test (funcall key a) (funcall key b)))))

  ;; the elements of the heap are stored in an adjustable array
  (elts (make-array 0 :adjustable t :fill-pointer t)))

;;; ----------------------------------------------------

(defmethod print-object ((h heap) stream)
  "Output a heap to a stream."
  (print-unreadable-object (h stream :type t)
    (format stream "~d item~:p" (length (heap-elts h)))))

;;; ----------------------------------------------------

(defun make-heap (test &key key initial-contents)
  "Create a heap with data in it."
  (let ((heap (%make-heap test :key key)))
    (prog1 heap
      (map nil #'(lambda (i) (heap-push i heap)) initial-contents))))

;;; ----------------------------------------------------

(defun heap-peek (heap)
  "Return the next element to be removed from the heap."
  (with-slots (elts)
    heap
    (when (plusp (length elts))
      (aref elts 0))))

;;; ----------------------------------------------------

(defun heap-flush (heap)
  "Remove all elements from the heap."
  (prog1 nil
    (setf (fill-pointer (heap-elts heap)) 0)))

;;; ----------------------------------------------------

(defun heap-contents (heap)
  "Return a copy of the heap items."
  (copy-seq (heap-elts heap)))

;;; ----------------------------------------------------

(defun heap-push (x heap)
  "Insert a new element onto the heap."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (compare elts)
    heap
    (loop
      with i = (vector-push-extend x elts)
      until (zerop i)

      ;; find the parent of this node
      for parent-i = (ash (1- i) -1)
      for parent = (aref elts parent-i)

      ;; up-shift
      do (if (funcall compare x parent)
           (progn
             (rotatef (aref elts i) (aref elts parent-i))
             (setf i parent-i))
           (loop-finish))

      ;; return the pushed item
      finally (return x))))

;;; ----------------------------------------------------

(defun heap-pop (heap)
  "Remove the root element from the heap."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (compare elts)
    heap
    (cond ((= (length elts) 0) nil)

          ;; only one element? just pop and return it
          ((= (length elts) 1) (vector-pop elts))

          ;; otherwise pop and shift-down
          (t (loop
               with i = 0

               ;; get the root element
               with root = (aref elts 0)

               ;; put the last element into the root
               with last = (setf (aref elts 0) (vector-pop elts))

               ;; find the two child indices of the root
               for ai = (+ (ash i 1) 1)
               for bi = (+ (ash i 1) 2)

               ;; get the children
               for a = (when (< ai (length elts)) (aref elts ai))
               for b = (when (< bi (length elts)) (aref elts bi))

               ;; choose the better child to test
               for ci = (cond ((and a b)
                               (if (funcall compare a b) ai bi))

                              ;; only one child is valid
                              (a ai)
                              (b bi))

               ;; shift down - no child? done
               do (cond ((null ci) (loop-finish))

                        ;; is the child better?
                        ((funcall compare (aref elts ci) last)
                         (progn
                           (rotatef (aref elts i) (aref elts ci))
                           (shiftf i ci)))

                        ;; done
                        (t (loop-finish)))

               ;; return the item popped from the heap
               finally (return root))))))
