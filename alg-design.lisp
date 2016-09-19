;;;; alg-design.lisp

(in-package #:alg-design)

;;; "alg-design" goes here. Hacks and glory await!

;;(ql:quickload :alg-design)
;;(in-package :alg-design)

;;(append '(a b) 'c) => (A B . C)
(defun append1 (lst obj)
  (append lst (list obj)))
 
 
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))


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

     
;;;
;1) list of verticies
;2) list of edges
;3) list of edges relate back to verticeis
;4) list of verticies relate back to edges
 
;;if edges only - like bit map of edges - then any change is O(m)
;;else if vertices that tell us each edege and weight then
(defun get-scc-file (path)
  (let ((lst nil))
    (with-open-file (str path :direction :input)
      (do ((line (read-line str nil 'eof)
                 (read-line str nil 'eof))
           (i 0 (incf i)))
          ((or (eql line 'eof)
               (> i 100)))
        (if (> (length line) 1)
            (let* ((curr-line (mapcar #'(lambda (x)
                                          (if (not (equalp "" x)) (parse-integer x)))
                                      (split-sequence #\space line)))
		   (tail (car curr-line))
		   (head (cadr curr-line)))
              (if (not (eq tail head))
		  (push (cons tail head) lst))))))
    lst))
 
(defparameter *g1* (get-scc-file "/Users/aliya/Downloads/SCC.txt"))
(defparameter *i* (make-hash-table)); nodes we've already seen 

(defparameter *leader-nodes* nil); list of leader nodes

(defparameter *s* nil);glob var for current soure vertex - used on second pass to calc leader nodes
(defparameter *t* 0); glob var for ordering on the first pass - used to calc finishing times

;;finishing time - try with hash table and list
(defparameter *fvs* (make-hash-table)); will be used for the correct ordering determined in the first pass
(defparameter *fvs-lst* nil)

(defun get-rev-g (g)
  (sort g #'< :key #'cdr))


(defun get-connecting-edges (g node fn)
  (


(defun dfs-loop (g)
  (setf *t* 0);for the first pass we need to set *t* to 0 - FOR FINISHING TIMES OF NODES - fvs
  (setf *s* nil); for second pass - current source vertex - FOR LEADERS
  (dolist (node g)
    (if (null (gethash node *i*)); if we haven't seen the node yet
	(progn
	  (push node *leader-nodes*);2nd pass means we haven't seen the node yet and therefore can't see the   
	  (dfs g n)))))

(defun dfs (g n)
  (let ((ns-connections (get-connecting-edges n)))
    (setf (gethash n *i*) t); means just mark the current node as seen - *i* holds those values 
    (dolist (node ns-connections);for each arc (i,j) in G if j not explored then (dfs G, j) - check if we've already seen it - on the sec
      (if (null (gethash node *i*))
	  (dfs g node)))
    (incf *t*)
    (add-to-fvs *t* n))); for the ordering


(defun add-to-fvs (val node)
  (setf (gethash *t*  *fvs*) node)
  (push node 
