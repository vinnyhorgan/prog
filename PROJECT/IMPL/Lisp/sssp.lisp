;;; Daniel Vincent Horgan - 928744

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

;; --- SEZIONE MINHEAP ---

(defun heap-rep-size (heap-rep)
  (third heap-rep))

(defun heap-rep-array (heap-rep)
  (fourth heap-rep))

(defun new-heap (heap-id &optional (initial-capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap
                  heap-id
                  0
                  (make-array (+ initial-capacity 1)
                              :initial-element nil
                              :adjustable t)))))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*)
  t)

(defun heap-empty (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (null heap-rep)
        t
        (= (heap-rep-size heap-rep) 0))))

(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))

(defun heap-head (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (and heap-rep (> (heap-rep-size heap-rep) 0))
        (aref (heap-rep-array heap-rep) 1)
        nil)))

(defun ensure-heap-capacity (heap-rep needed-index)
  (let ((array (heap-rep-array heap-rep)))
    (when (>= needed-index (length array))
      (let ((new-len (max (+ needed-index 1) (* 2 (length array)))))
        (setf (fourth heap-rep)
              (adjust-array array
                            new-len
                            :initial-element nil))))))

(defun heap-swap (heap-rep i j)
  (let ((array (heap-rep-array heap-rep)))
    (rotatef (aref array i) (aref array j))))

(defun heapify-up (heap-rep index)
  (loop while (> index 1) do
        (let* ((parent (floor index 2))
               (array (heap-rep-array heap-rep))
               (entry (aref array index))
               (parent-entry (aref array parent)))
          (if (< (first entry) (first parent-entry))
              (progn
                (heap-swap heap-rep index parent)
                (setf index parent))
              (return)))))

(defun heapify-down (heap-rep index)
  (let* ((size (heap-rep-size heap-rep))
         (left (* 2 index))
         (right (+ left 1)))
    (when (<= left size)
      (let ((smallest left)
            (array (heap-rep-array heap-rep)))
        ;; Attenzione qui: ultimo nodo interno puo' avere solo figlio sinistro.
        (when (and (<= right size)
                   (< (first (aref array right))
                      (first (aref array left))))
          (setf smallest right))
        (when (< (first (aref array smallest))
                 (first (aref array index)))
          (heap-swap heap-rep index smallest)
          (heapify-down heap-rep smallest))))))

(defun heap-insert (heap-id k v)
  (let* ((heap-rep (new-heap heap-id))
         (new-size (+ (heap-rep-size heap-rep) 1)))
    (ensure-heap-capacity heap-rep new-size)
    (setf (third heap-rep) new-size)
    (setf (aref (heap-rep-array heap-rep) new-size)
          (list k v))
    (heapify-up heap-rep new-size)
    t))

(defun heap-extract (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (when (and heap-rep (> (heap-rep-size heap-rep) 0))
      (let* ((size (heap-rep-size heap-rep))
             (array (heap-rep-array heap-rep))
             (min-entry (aref array 1)))
        (if (= size 1)
            (progn
              (setf (aref array 1) nil)
              (setf (third heap-rep) 0))
            (progn
              (setf (aref array 1) (aref array size))
              (setf (aref array size) nil)
              (setf (third heap-rep) (- size 1))
              (heapify-down heap-rep 1)))
        min-entry))))

(defun heap-find-index (heap-rep old-key value)
  (let ((array (heap-rep-array heap-rep))
        (size (heap-rep-size heap-rep))
        (found nil))
    (loop for i from 1 to size do
          (let ((entry (aref array i)))
            (when (and (equal (first entry) old-key)
                       (equal (second entry) value))
              (setf found i)
              (return))))
    found))

(defun heap-modify-key (heap-id new-key old-key value)
  (let* ((heap-rep (gethash heap-id *heaps*))
         (index (and heap-rep
                     (heap-find-index heap-rep old-key value))))
    (when index
      (setf (first (aref (heap-rep-array heap-rep) index)) new-key)
      (cond ((< new-key old-key)
             (heapify-up heap-rep index))
            ((> new-key old-key)
             (heapify-down heap-rep index)))
      (return-from heap-modify-key t))
    nil))

(defun heap-print (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (null heap-rep)
        (format t "~%Heap ~A inesistente.~%" heap-id)
        (let ((size (heap-rep-size heap-rep))
              (array (heap-rep-array heap-rep)))
          (format t "~%Heap ~A (size ~A):~%" heap-id size)
          (loop for i from 1 to size do
                (format t "  [~A] ~A~%" i (aref array i)))))
    t))

;; --- SEZIONE GRAFI ---

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (is-graph graph-id)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun remove-keys-by-test (table test-fn)
  (let ((keys-to-remove '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (funcall test-fn key)
                 (push key keys-to-remove)))
             table)
    (dolist (key keys-to-remove)
      (remhash key table))))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (remove-keys-by-test
   *vertices*
   (lambda (key)
     (and (consp key)
          (equal (first key) 'vertex)
          (equal (second key) graph-id))))
  (remove-keys-by-test
   *arcs*
   (lambda (key)
     (and (consp key)
          (equal (first key) 'arc)
          (equal (second key) graph-id))))
  (remove-keys-by-test
   *visited*
   (lambda (key)
     (and (consp key)
          (equal (first key) graph-id))))
  (remove-keys-by-test
   *distances*
   (lambda (key)
     (and (consp key)
          (equal (first key) graph-id))))
  (remove-keys-by-test
   *previous*
   (lambda (key)
     (and (consp key)
          (equal (first key) graph-id))))
  nil)

(defun new-vertex (graph-id vertex-id)
  (new-graph graph-id)
  (let ((vertex-rep (list 'vertex graph-id vertex-id)))
    (setf (gethash vertex-rep *vertices*) vertex-rep)))

(defun graph-vertices (graph-id)
  (let ((result '()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (and (consp value)
                          (equal (first value) 'vertex)
                          (equal (second value) graph-id))
                 (push value result)))
             *vertices*)
    (nreverse result)))

(defun new-arc (graph-id u v &optional (weight 1))
  (unless (and (numberp weight) (>= weight 0))
    (error "Peso non valido: ~A" weight))
  (new-vertex graph-id u)
  (new-vertex graph-id v)
  (let ((key (list 'arc graph-id u v))
        (arc-rep (list 'arc graph-id u v weight)))
    (setf (gethash key *arcs*) arc-rep)
    arc-rep))

(defun graph-arcs (graph-id)
  (let ((result '()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (and (consp value)
                          (equal (first value) 'arc)
                          (equal (second value) graph-id))
                 (push value result)))
             *arcs*)
    (nreverse result)))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((result '()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (and (consp value)
                          (equal (first value) 'arc)
                          (equal (second value) graph-id)
                          (equal (third value) vertex-id))
                 (push value result)))
             *arcs*)
    (nreverse result)))

(defun graph-print (graph-id)
  (format t "~%Grafo ~A~%" graph-id)
  (format t "Vertici: ~A~%" (graph-vertices graph-id))
  (format t "Archi: ~A~%" (graph-arcs graph-id))
  nil)

;; --- SEZIONE SSSP ---

(defun sssp-dist (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *distances*))

(defun sssp-visited (graph-id vertex-id)
  (not (null (gethash (list graph-id vertex-id) *visited*))))

(defun sssp-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf (gethash (list graph-id vertex-id) *distances*) new-dist)
  nil)

(defun sssp-change-previous (graph-id vertex-id previous-vertex)
  (setf (gethash (list graph-id vertex-id) *previous*) previous-vertex)
  nil)

(defun sssp-clear-table (table graph-id)
  (remove-keys-by-test
   table
   (lambda (key)
     (and (consp key)
          (equal (first key) graph-id)))))

(defun sssp-clear-state (graph-id)
  (sssp-clear-table *distances* graph-id)
  (sssp-clear-table *visited* graph-id)
  (sssp-clear-table *previous* graph-id))

(defun sssp-relax-neighbors (graph-id heap-id vertex dist)
  ;; NB: se la distanza e' "infinita", non provo nemmeno i rilassamenti.
  (unless (eql dist most-positive-double-float)
    (dolist (arc-rep (graph-vertex-neighbors graph-id vertex))
      (let ((neighbor (fourth arc-rep))
            (weight (fifth arc-rep)))
        (unless (sssp-visited graph-id neighbor)
          (let* ((old-dist (sssp-dist graph-id neighbor))
                 (new-dist (+ dist weight)))
            (when (< new-dist old-dist)
              (sssp-change-dist graph-id neighbor new-dist)
              (sssp-change-previous graph-id neighbor vertex)
              ;; Decrease-key lineare: cerco (old-key, value) nello heap.
              (heap-modify-key heap-id
                               new-dist
                               old-dist
                               neighbor))))))))

(defun sssp-dijkstra (graph-id source)
  (sssp-clear-state graph-id)
  (let* ((vertices (graph-vertices graph-id))
         (source-present nil)
         (heap-id (list 'sssp-heap graph-id)))
    (dolist (vertex vertices)
      (when (equal (third vertex) source)
        (setf source-present t)))
    (unless source-present
      (return-from sssp-dijkstra nil))
    ;; Pulizia extra: evita residui se una run precedente si era interrotta.
    (heap-delete heap-id)
    (new-heap heap-id (+ 10 (length vertices)))
    (dolist (vertex vertices)
      (let ((v (third vertex)))
        (if (equal v source)
            (progn
              (sssp-change-dist graph-id v 0)
              (heap-insert heap-id 0 v))
            (progn
              (sssp-change-dist graph-id v most-positive-double-float)
              (heap-insert heap-id most-positive-double-float v)))))
    (loop while (heap-not-empty heap-id) do
          (let ((entry (heap-extract heap-id)))
            (when entry
              (let ((dist (first entry))
                    (vertex (second entry)))
                (unless (sssp-visited graph-id vertex)
                  (setf (gethash (list graph-id vertex) *visited*) t)
                  (sssp-relax-neighbors graph-id
                                        heap-id
                                        vertex
                                        dist))))))
    (heap-delete heap-id)
    nil))

(defun sssp-build-path (graph-id source current acc)
  (if (equal current source)
      acc
      (let* ((prev (sssp-previous graph-id current))
             (arc-rep (and prev
                           (gethash (list 'arc graph-id prev current)
                                    *arcs*))))
        (if (and prev arc-rep)
            (sssp-build-path graph-id
                             source
                             prev
                             (cons arc-rep acc))
            nil))))

(defun sssp-shortest-path (graph-id source vertex-id)
  (if (equal source vertex-id)
      nil
      (sssp-build-path graph-id source vertex-id '())))
