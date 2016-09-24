;;;; alg-design.lisp

(in-package #:alg-design)

;;; "alg-design" goes here. Hacks and glory await!

;;to quickly load
;;(ql:quickload :alg-design)
;;(in-package :alg-design)

;;for github
;;create new repo
;git init
;;add to staging area and then commit to staging
;git add README.md
;OR FOR ALL FILES - git add . 
;git commit -m "first commit"
;;push the repo (can also just use this to push an existing repo)
;git remote add origin https://github.com/ajivani/algs.git
;git push -u origin 


;;(append '(a b) 'c) => (A B . C)
(defun append1 (lst obj)
  (append lst (list obj)))
 
 
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


(defparameter *graph* (make-hash-table))
(defparameter *graph-rev* (make-hash-table)) ;;will store the reverse 

(defparameter *i* (make-hash-table)); nodes we've already seen 

(defparameter *leader-nodes* nil); list of leader nodes
(defparameter *leader-nodes-hash* (make-hash-table));

(defparameter *s* nil);glob var for current soure vertex - used on second pass to calc leader nodes
(defparameter *t* 0); glob var for ordering on the first pass - used to calc finishing times

;;finishing time - try with hash table and list
(defparameter *fvs* (make-hash-table)); will be used for the correct ordering determined in the first pass
(defparameter *fvs-lst* nil)


;;this seems to work well
(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))


;;destructive funtioon updates *graph* and *graph-vertex*
(defun get-scc-file2 (path)
  (let ((lst nil))
    (with-open-file (str path :direction :input)
      (do ((line (read-line str nil 'eof)
                 (read-line str nil 'eof))
           (i 0 (incf i)))
          ((or (eql line 'eof)
               (> i 1000000000)))
        (if (> (length line) 1)
            (let* ((curr-line (mapcar #'(lambda (x)
                                          (if (not (equalp "" x)) (parse-integer x)))
                                      (split-sequence #\space line)))
                   (tail (car curr-line))
                   (head (cadr curr-line))
                   (vertex-found? (gethash tail *graph*))
                   (rev-vertex-found? (gethash head *graph-rev*)))
              (if (not (eq tail head))
                  (progn
                    (make-graph *graph* vertex-found? tail head); makes regular graph
                    (make-graph *graph-rev* rev-vertex-found? head tail); does everything backwards
                    ))))))))


;just to test
(maphash #'(lambda (k v)
                      (format t "~a:~a~%" k v))
	 *graph*)

;(get-scc-file2 "/Users/aliya/Downloads/SCC.txt")

(defun make-graph (g vertex-found? tail head)
  (if (null vertex-found?)
      (setf (gethash tail g) (list head))
      (setf (gethash tail g) (push head vertex-found?))))


;(defun make-edges (edges

(defun run-test-alg (&optional (path "/Users/aliya/Downloads/SCC.txt"))
  (setf *graph* (make-hash-table))
  (setf *graph-rev* (make-hash-table))
  (setf *i* (make-hash-table))
  (setf *leader-nodes* nil)
  (setf *leader-nodes-hash* (make-hash-table))
  (setf *s* nil)
  (setf *t* 0)
  (setf *fvs* (make-hash-table))
  (setf *fvs-lst* nil)
  (get-scc-file2 path) ;populates *graph* and *graph-rev*
  (dfs-loop *graph-rev*) ;calculate finishing times on the reverse graph
  (setf *i* (make-hash-table))
  (setf *s* nil)
  (setf *leader-nodes-hash* (make-hash-table))
  (setf *leader-nodes* nil)
  (dfs-loop *graph*)
  (scc-counter2 *leader-nodes-hash*))



(defun get-connecting-edges (g node)
  (gethash node g))


(defun dfs-loop (g)
  (setf *t* 0);for the first pass - FOR FINISHING TIMES OF NODES - fvs
  (setf *s* nil); for second pass -  FOR LEADERS - current source vertex
  (cond ((null *fvs-lst*); first pass
         (maphash #'(lambda (k v)
                      (if (null (gethash k *i*))
                          (dfs g k)))
                  g))
        (t ;for the second pass
         ;(break)
         (dolist (node *fvs-lst*) ;the finishing times should be calculated in backwards orders
           (if (null (gethash node *i*)); if we haven't seen the node yet
               (progn
                 (setf *s* node)
                 ;(push node *leader-nodes*);2nd pass means we haven't seen the node yet and therefore can't see the   
                 (dfs g node)))))))

(defun dfs (g n)
  (let ((ns-connections (get-connecting-edges g n))); n's connections
    ;(push *s* *leader-nodes*); for second pass
    (add-leader-nodes *s*)
    (setf (gethash n *i*) t); mark the current node as seen - *i* is nodes we've already seen 
    (dolist (node ns-connections);for each arc (i,j) in G if j not explored then (dfs G, j) - check if we've already seen it
      (if (null (gethash node *i*))
	  (dfs g node)))
    (incf *t*) ;for first pass
    (add-to-fvs *t* n))); for the ordering


;;adds it to a hash and a list just to test which way is faster
(defun add-to-fvs (val node)
  (setf (gethash val *fvs*) node); finishing value of that node is the current val of *t* ; actually do it backwards then it's easy to pick the max *t* and go down to 1
  (push node *fvs-lst*))

(defun add-leader-nodes (node)
  (if (null (gethash node *leader-nodes-hash*))
      (setf (gethash node *leader-nodes-hash*) 1); meaning one node in the scc
      (incf (gethash node *leader-nodes-hash*)))); increment the size by 1

;say we are counting by just having a stupid list instead of a hashtable 
(defun scc-counter (scc-leaders)
  (let ((scc-size nil)
        (nodes-seen? (make-hash-table)))    
    (dolist (n scc-leaders)
      (when (null (gethash n nodes-seen?))
        (push (count n scc-leaders) scc-size)
        (setf (gethash n nodes-seen?) t)))
    (sort scc-size #'>)))

(defun scc-counter2 (scc-leaders-hash)
  (let ((scc-size nil))    
    (maphash #'(lambda (k v)
		 (if (> v 100) (push v scc-size)))
	     scc-leaders-hash)		 
    (sort scc-size #'>)))
