"
  defparser: Parsing-Expression-Grammar Parser Generator Macro


  Usage:
  (defparser parser-name (start-symbols ...)
    parsing-rules ...) 
      =>  define parser-name as a functon
          (parser-name sequence :start-index index)
            => result, next-index, matched-p, exhausted-p

  parser-name:       name of the parser function
  start-symbols ...: a list of start symbols (parser will try
                     sequentially until success)]
  parsing-rules ...: parsing rules of following format.

    (non-terminal-symbol  rule  interpretation-function) ...

  A rule is a composition of following PEG expressions,

    (e1 e2 ...)   : sequence
    (/ e1 e2 ...) : ordered choice
    (repeat e)    : zero-or-more
    (? e)         : optional
    (& e)         : 'and' predicate
    (! e)         : 'not' predicate

  a result-grouping sequence that matches exactly the same
  as the sequence expression but return results as a single list,

  (group e1 ...)  : grouped sequence

  calling external parsers,

    (parser p)    : external parser-function call

  and following predicates for terminal symbols.

    '(predicate)  : => (predicate x)
    'symbol       : => (eq 'symbol x)
    literal       : => (eql literal x)
    #(a b c)      : sequentially match to vector elements (aka. start with)
    '#(a b c)     : match to the vector itself (by equal)
    any           : any kinds of an input element
    nil           : empty input (does not consume the input sequence)
    $             : the end of the input

  The ordered choice is similar to a BNF's choice expression, but
  its evaluation is short-circuit way, i.e., it tries expressions from
  the first one to the last, and returns only the first successful match.

  Syntax of the parser function p must be as follows.

    (p sequence :start-index index)
      => result, next-index, matched-p[, exhausted-p: ignored]

  The 'and' and 'not' predicates are syntactic; these predicates check
  if an input sequence satisfies their subexpressions, then just return
  success and fail, respectively, without consuming the input sequence.


  Example:
  (peg:defparser non-grouping (A)
    (A  (#\a #\a (repeat #\a))
        #'list))

  (peg:defparser with-grouping (A)
    (A  (#\a (group #\a (repeat #\a)))
        #'list))

  (nth-value 0 (non-grouping \"aaaa\"))
  => (#\a #\a #\a #\a)

  (nth-value 0 (with-grouping \"aaaa\"))
  => (#\a (#\a #\a #\a))

  ;; S <- &(A c) a a* B$
  ;; A <- a A? b
  ;; B <- b B? c
  (peg:defparser non-cf (S)
    (S  ((& (A #\c)) #\a (repeat #\a) B $))
    (A  (#\a (? A) #\b))
    (B  (#\b (? B) #\c))

  (non-cf \"aaabbbccc\")
  => T  ;; result (can be a partial match)
     9  ;; next sequence index
     T  ;; partial-match status (t: success, nil: failure)
     T  ;; input exhausted
  (non-cf \"aaabbbbccc\")
  => NIL
     0
     NIL
     NIL
"

(defpackage :dtakahashi.peg
  (:use :common-lisp)
  (:export :defparser)
  (:nicknames :peg))


(in-package :dtakahashi.peg)

;; Sequence: sequence of one or more exprs
;; PEG: expr1 expr2 ... => (expr1 exprs2 ...)
(defun sequence-expressions (seq current-result current-index exprs)
  (let ((next-index     (gensym))
        (updated-result (gensym))
        (success        (gensym)))
    `(let ((,next-index ,current-index) (,updated-result ,current-result))
       (block nil
           ,(reduce (lambda (translated-expression success-continuation)
                      `(multiple-value-bind (,updated-result ,next-index ,success)
                           ,translated-expression
                         (if ,success
                             ,success-continuation
                             (return (values ,updated-result ,current-index nil)))))
                    (mapcar (lambda (expr)
                              (translate-expression seq updated-result next-index
                                                    expr))
                            exprs)
                    :from-end t
                    :initial-value `(return (values ,updated-result ,next-index t)))))))


;; Group: similar to the "sequence" expressions but combine its result values into a list
(defun group-expressions (seq current-result current-index exprs)
  (let ((next-index     (gensym))
        (updated-result (gensym))
        (success        (gensym)))
    `(let ((,next-index ,current-index) (,updated-result nil))
       (block nil
           ,(reduce (lambda (translated-expression success-continuation)
                      `(multiple-value-bind (,updated-result ,next-index ,success)
                           ,translated-expression
                         (if ,success
                             ,success-continuation
                             (return (values ,updated-result ,current-index nil)))))
                    (mapcar (lambda (expr)
                              (translate-expression seq updated-result next-index
                                                    expr))
                            exprs)
                    :from-end t
                    :initial-value `(return (values (cons (nreverse ,updated-result) ,current-result)
                                                    ,next-index
                                                    t)))))))


;; Optional:
;; PEG: expr? => (? expr)
(defun optional-expression (seq current-result current-index
                            expr)
  (let ((next-index     (gensym))
       	(updated-result (gensym))
        (success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
	 ,(translate-expression seq current-result current-index
                                expr)
       (if ,success
	   (values ,updated-result
		   ,next-index
		   t)
	   (values (cons nil ,current-result)
		   ,current-index
		   t)))))


;; Zero-or-more:
;; PEG: expr* => (repeat expr...) (Note: implicit sequence)
(defun repeat-of-an-expression (seq current-result current-index exprs)
  (let ((index          (gensym))
	(next-index     (gensym))
        (result         (gensym))
	(updated-result (gensym))
        (success        (gensym)))
    `(do ((,result ,current-result)
          (,index ,current-index))
         (nil)
       (declare (type fixnum ,index))
       (multiple-value-bind (,updated-result ,next-index ,success)
           ,(translate-expression seq result index exprs)
         (declare (type fixnum ,next-index))
         (if ,success
             (progn
               (setf ,result ,updated-result)
               (setf ,index ,next-index))
             (return (values ,result ,index t)))))))


;; Ordered choice:
;; PEG: expr1 / expr2 / ... => (/ expr1 expr2 ...)
(defun ordered-choice-expressions (seq current-result current-index exprs)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(block nil
       ,(reduce (lambda (translated-expression failure-continuation)
                  `(multiple-value-bind (,updated-result ,next-index ,success)
                       ,translated-expression
                     (if ,success
                         (return (values ,updated-result ,next-index t))
                         ,failure-continuation)))
                (mapcar (lambda (expression)
                          (translate-expression seq
                                                current-result
                                                current-index
                                                expression))
                        exprs)
                :from-end t
                :initial-value `(values ,updated-result ,current-index nil)))))


;; "And" predicate:
;; PRG: &expr => (and expr)
(defun and-predicate (seq current-result current-index expr)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
         ,(translate-expression seq
				current-result
				current-index
				expr)
       (declare (ignore ,updated-result ,next-index))
       (if ,success
           (values ,current-result ,current-index t)
           (values nil ,current-index nil)))))


;; "Not" predicate:
;; PRG: !expr => (not expr)
(defun not-predicate (seq current-result current-index expr)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
         ,(translate-expression seq
				current-result
				current-index
				expr)
       (declare (ignore ,updated-result ,next-index))
       (if (not ,success)
           (values ,current-result
		   ,current-index
		   t)
           (values nil
		   ,current-index
		   nil)))))


;; Three predicate forms for terminal symbols
;; 1: '(predicate-call) -> (predicate-call x)
(defun predicate-call (seq current-result current-index predicate-call)
  `(if (and (< ,current-index (length ,seq))
	    (,@predicate-call (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index) ,current-result)
	       (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
	       t)
       (values (make-array 2 :initial-contents
                           (list ,current-index ',predicate-call))
               ,current-index
               nil)))

;; 2: 'symbol == '(eq 'symbol x)
(defun predicate-eq (seq current-result current-index symbol)
  `(if (and (< ,current-index (length ,seq))
              (eq ,symbol (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
	       (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
	       t)
       (values (cons ,current-index '(eq ,symbol))
	       ,current-index
	       nil)))

;; 3: literal == '(eql literal x)
(defun predicate-eql (seq current-result current-index literal)
  `(if (and (< ,current-index (length ,seq))
	    (eql ,literal (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
	       (1+ ,current-index)
	       t)
       (values (cons ,current-index '(eql ,literal))
	       ,current-index
	       nil)))

;; 4: 'literal == '(equal literal x)
(defun predicate-equal (seq current-result current-index literal)
  `(if (and (< ,current-index (length ,seq))
	    (equal ,literal (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
	       (1+ ,current-index)
	       t)
       (values (cons ,current-index '(equal ,literal))
	       ,current-index
	       nil)))

(defun predicate-expression (seq current-result current-index expr)
  (cond
    ((symbolp expr)
     (predicate-eq    seq current-result current-index expr))
    ((consp expr)
     (predicate-call  seq current-result current-index expr))
    (t
     (predicate-equal seq current-result current-index expr))))


;; "*": always success with consuming a single input element
(defun any-element (seq current-result current-index)
  `(if (< ,current-index (length ,seq))
       (values (cons (elt ,seq ,current-index) ,current-result)
               (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
               t)
       (values (cons ,current-index 'unexpected-end-of-input)
	       ,current-index
	       nil)))


;; "nil": always success without consuming the input sequence
(defun empty-element (seq current-result current-index)
  (declare (ignore seq))
  `(values ,current-result ,current-index t))


;; $: success if the current index is out or range
(defun end-element (seq current-result current-index)
  `(if (>= ,current-index (length ,seq))
       (values ,current-result
               ,current-index
               t)
       (values (cons ,current-index 'unterminated)
	       ,current-index
	       nil)))


;; parser: call another parser
(defun call-external-parser (seq current-result current-index parser)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    ;; use a given parser for a partial match
    ;; (ignore exhausted-p of retvalues)
    `(multiple-value-bind (,updated-result ,next-index ,success)
	 (,parser ,seq :start-index ,current-index)
       (if ,success
           (let ((,next-index (locally
                                  ;; stop warning about
                                  ;; potentially failing coercion
                                  (declare #+sbcl(sb-ext:muffle-conditions
                                                  sb-ext:compiler-note))
                                (coerce ,next-index 'fixnum))))
             (values (cons ,updated-result ,current-result)
                     ,next-index
                     t))
	   (values ,updated-result
		   ,current-index
		   nil)))))


(defun vector-sig-generalize-length (type-signature)
  (cond
    ((= (length type-signature) 1)
     `(,(car type-signature)))
    ((= (length type-signature) 2)
     `(,(car type-signature) *))
    ((not (consp (caddr type-signature)))
     `(,(car type-signature) ,(cadr type-signature) *))
    (t
     `(,(car type-signature) ,(cadr type-signature)
        ,(make-list (length (caddr type-signature)) :initial-element '*)))))

;; vector-literal: match when the input sequence starts with a given vector
(defun match-sub-vector (seq current-result current-index sub-vector)
  (let ((i             (gensym))
        (j             (gensym))
        (target        (gensym))
        (vector-length (length sub-vector)))
    `(let ((,target ,sub-vector))
       (if (>= (- (length ,seq) ,current-index) ,vector-length)
           (do ((,i ,current-index (1+ ,i))
                (,j 0              (1+ ,j)))
               ((= ,j ,vector-length)
                (values (cons (coerce (subseq ,seq ,current-index ,i)
                                      (quote ,(vector-sig-generalize-length
                                               (type-of sub-vector))))
                              ,current-result)
                        ,i
                        t))
             (if (not (eql (elt ,seq ,i) (elt ,target ,j)))
                 (return (values (cons ,current-index '(start-with ,sub-vector))
                                 ,current-index
                                 nil))))
           (values (cons ,current-index '(start-with ,sub-vector))
                   ,current-index ,nil)))))


(defun translate-expression (seq current-result current-index expr)
  (cond
    ;; Use string comparisons (instead of symbol eqs) to
    ;; ignore namespaces.
    ;; These comparisons run only at compilation time, so
    ;; there are no performance penalty at runtime.
    ((symbolp expr)
     (cond
       ;; '*' matches to any symbols
       ((equal (symbol-name expr) "*")
        (any-element   seq current-result current-index))
       
       ;; 'nil' matches to an 'empty' input
       ((eq expr nil)
        (empty-element seq current-result current-index))

       ;; '$' matches to the out of the input
       ((equal (symbol-name expr) "$")
        (end-element   seq current-result current-index))
       
       ;; match to a non-terminal symbol
       (t
        (let ((match-result (gensym))
	      (next-index   (gensym))
	      (success      (gensym)))
          `(multiple-value-bind (,match-result ,next-index ,success)
               (,expr ,seq nil ,current-index)
             (declare (type fixnum ,next-index))
             (if ,success
                 (values (cons ,match-result ,current-result)
			 ,next-index
			 t)
                 (values ,match-result
			 ,next-index
			 nil)))))))
    
    ;; match a given expression to one of the PEG syntactic rules
    ((consp expr)
     (let ((head (car expr)))
       (if (symbolp head)
           (let ((head-name (symbol-name head)))
             (cond
               ;; Expressions that take a single sub-expression
               ((equal head-name "QUOTE")
                (predicate-expression       seq current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "PARSER")
                (call-external-parser       seq current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "?")
                (optional-expression        seq current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "REPEAT") ;; Note: implicit sequence
                (repeat-of-an-expression    seq current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "&")
                (and-predicate              seq current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "!")
                (not-predicate              seq current-result current-index
                                            (cadr expr)))
               
               ;; Expressions that may take n sub-expressions
               ((equal head-name "/")
                (ordered-choice-expressions seq current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "GROUP")
                (group-expressions          seq current-result current-index
                                            (cdr expr)))
               
               (t
                (sequence-expressions       seq current-result current-index
                                            expr))))
           
           ;; A sequence expression of which head is not a symbol (literal ...)
           (sequence-expressions      seq current-result current-index
                                      expr))))

    ;; A starts-with-a-vector expression
    ((vectorp expr)
     (match-sub-vector     seq current-result current-index
                           expr))
    
    ;; Otherwise, assume this is a scalar literal and apply an eql comparison
    (t
     (predicate-eql       seq current-result current-index
                          expr))))


(defun translate-single-rule (nt-symbol rule-body memo result-interpretor)
  (let ((seq            (gensym))
        (current-result (gensym))
        (updated-result (gensym))
        (current-index  (gensym))
        (next-index     (gensym))
	(success        (gensym))
        (lookup-result  (gensym))
        (lookup-success (gensym)))
    `(,nt-symbol (,seq ,current-result ,current-index)
                 (declare (type simple-vector ,seq)
                          (type fixnum ,current-index))
                 (multiple-value-bind (,lookup-result ,lookup-success)
                     (gethash ,current-index ,memo)
                   (if ,lookup-success
                       (locally
                           (declare (type (simple-vector 3) ,lookup-result))
                         (values (elt ,lookup-result 0)
                                 (elt ,lookup-result 1)
                                 (elt ,lookup-result 2)))
                       (progn
                         ;; add a sentinel to reject reentrant evaluations
                         ;; (maybe left recursions?)
                         (setf (gethash ,current-index ,memo)
                               (make-array 3 :initial-contents
                                           (list (cons ,current-index 'left-recursion)
                                                 ,current-index
                                                 nil)))
                         (multiple-value-bind (,updated-result ,next-index ,success)
                             ,(translate-expression seq
                                                    current-result
                                                    current-index
                                                    rule-body)
                           (declare (type fixnum ,next-index) (type boolean ,success))
                           (let ((,updated-result (if ,success
                                                      (apply ,result-interpretor
                                                             (nreverse ,updated-result))
                                                      ,updated-result)))
                             (let ((,lookup-result (gethash ,current-index ,memo)))
                               (declare (type (simple-vector 3) ,lookup-result))
                               (setf (elt ,lookup-result 0) ,updated-result
                                     (elt ,lookup-result 1) ,next-index
                                     (elt ,lookup-result 2) ,success)
                               (values ,updated-result ,next-index ,success))))))))))

(defun set-find (key s)
  (let ((m (member key s)))
    (if (consp m)
        (values (car m) t)
        (values nil nil))))

(defun set-add (s key)
  (cons key s))

(defun tree-mapc1 (function tree)
  (if (consp tree)
      (dolist (i tree)
        (tree-mapc1 function i))
      (funcall function tree)))

(defun build-non-terminal-symbol-dependency (non-terminal-symbols rules)
  (let ((h (make-hash-table)))
    (dolist (sym non-terminal-symbols)
      (setf (gethash sym h) nil))
    (mapc (lambda (non-term-sym rule)
            (tree-mapc1 (lambda (sym)
                          (if (nth-value 1 (gethash sym h))
                              ;; sym is a non-terinal symbol
                              (setf (gethash non-term-sym h)
                                    (cons sym (gethash non-term-sym h)))))
                        rule))
          non-terminal-symbols
          rules)
    h))

(defun collect-inlinable (start-symbols non-terminal-symbols rules)
  (let ((sym-deps (build-non-terminal-symbol-dependency
                   non-terminal-symbols rules))
        (not-inlinable (make-hash-table))
        (traversed     (make-hash-table)))
    (labels ((traverse (current path-from-start)
               (setf (gethash current traversed) t)
               (if (nth-value 1 (set-find current path-from-start))
                   (setf (gethash current not-inlinable) t)
                   (dolist (next (gethash current sym-deps))
                     (traverse next (set-add path-from-start current))))))
      (dolist (start start-symbols)
        (traverse start nil))
      (loop for symbol being the hash-keys of traversed
         when (not (nth-value 1 (gethash symbol not-inlinable)))
         collect symbol))))


(defmacro defparser (name start-symbols &body rules)
  (let ((input-sequence (gensym))
        (input-length   (gensym))
        (process        (gensym))
        (start-index    (gensym))
        (result         (gensym))
        (index-next     (gensym))
        (success        (gensym))
        (non-terminal-symbols (mapcar #'car   rules))
        (rule-bodies          (mapcar #'cadr  rules))
        (result-interpretors  (mapcar #'caddr rules))
	(default-interpretor  (gensym))
        (memoise-tables (make-hash-table)))
    (let ((inlinable-symbols (collect-inlinable start-symbols
                                                non-terminal-symbols
                                                rule-bodies)
            ))
      (mapc (lambda (non-terminal)
              (setf (gethash non-terminal memoise-tables)
                    (gensym)))
            non-terminal-symbols)
      `(let ((,default-interpretor (lambda (&rest xs) (declare (ignore xs)) t)))
         (declare  (ignorable ,default-interpretor)
                   #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note)
                   (optimize (speed 3)))
         (labels ((,process (,input-sequence ,start-index)
                    (declare (type simple-vector ,input-sequence)
                             (type fixnum ,start-index))
                    ;; generate memoise tables for each non-terminal symbol
                    (let ,(mapcar (lambda (memo-var)
                                    `(,memo-var (make-hash-table)))
                                  (loop for value being the hash-value of memoise-tables
                                     collect value))
                      ;; main recursive parser functions
                      (labels ,(mapcar (lambda (non-terminal-symbol rule result-interpretor)
                                         (translate-single-rule non-terminal-symbol
                                                                rule
                                                                (gethash non-terminal-symbol
                                                                         memoise-tables)
                                                                (or result-interpretor
                                                                    default-interpretor)))
                                       non-terminal-symbols
                                       rule-bodies
                                       result-interpretors)
                        (declare ,@(if inlinable-symbols `((inline ,@inlinable-symbols))))
                        (let ((,input-length (length ,input-sequence)))
                          (or
                           ,@(mapcar (lambda (start-symbol)
                                       `(multiple-value-bind (,result ,index-next ,success)
                                            (,start-symbol ,input-sequence nil ,start-index)
                                          (declare (type fixnum ,index-next))
                                          (if ,success
                                              (values ,result     ;; parse result
                                                      ,index-next ;; next sequence index
                                                      t           ;; (partially-)matched-p
                                                      (= ,input-length ,index-next)) ;; exhausted-p
                                              (values ,result ;; maybe an error descripting object
                                                      ,index-next
                                                      nil
                                                      nil))))
                                     start-symbols)))))))
           (defun ,name (input-sequence &key (start-index 0))
             (let ((start-index    (locally
                                       ;; stop warning about
                                       ;; potentially failing coercion
                                       (declare #+sbcl(sb-ext:muffle-conditions
                                                       sb-ext:compiler-note))
                                     (coerce start-index 'fixnum)))
                   (input-sequence (locally
                                       ;; same as above
                                       (declare #+sbcl(sb-ext:muffle-conditions
                                                       sb-ext:compiler-note))
                                     (coerce input-sequence 'simple-vector))))
               (,process input-sequence start-index))))))))
