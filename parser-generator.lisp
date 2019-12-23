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
  (:export :defparser :*optimization-flag*)
  (:nicknames :peg))


(in-package :dtakahashi.peg)

(defstruct peg-failure-descriptor
  (pos 0 :type fixnum)
  fail)

;; Sequence: sequence of one or more exprs
;; PEG: expr1 expr2 ... => (expr1 exprs2 ...)
(defun sequence-expressions (seq seq-length current-result current-index exprs)
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
                             (return (values ,updated-result ,current-index ,success)))))
                    (mapcar (lambda (expr)
                              (translate-expression seq seq-length updated-result next-index
                                                    expr))
                            exprs)
                    :from-end t
                    :initial-value `(return (values ,updated-result ,next-index ,success)))))))


;; Group: similar to the "sequence" expressions but combine its result values into a list
(defun group-expressions (seq seq-length current-result current-index exprs)
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
                             (return (values ,updated-result ,current-index ,success)))))
                    (mapcar (lambda (expr)
                              (translate-expression seq seq-length updated-result next-index
                                                    expr))
                            exprs)
                    :from-end t
                    :initial-value `(return (values (cons (nreverse ,updated-result) ,current-result)
                                                    ,next-index
                                                    ,success)))))))


;; Let: bind the first match to a symbol, and use it in successive sequential matches
;; (let (x <alpabet>) ", " x) -> "a, a", "b, b", ...
(defun let-expression (seq seq-length current-result current-index exprs)
  (Let ((next-index     (gensym))
        (updated-result (gensym))
        (success        (gensym))
        (matched-seq    (gensym))
        (matched-result (gensym))
        (bound-seq      (gensym))
        (bound-seq-len  (gensym))
        (bound-index    (gensym)))
    (destructuring-bind ((bind-sym bound-expr) &rest exprs)
        exprs
      `(multiple-value-bind (,matched-result ,next-index ,success)
           ,(translate-expression seq seq-length nil current-index
                                  bound-expr)
         (if ,success
             (let ((,matched-seq (subseq ,seq ,current-index ,next-index))
                   (,updated-result (cons ,matched-result ,current-result)))
               (flet ((,bind-sym (,bound-seq ,bound-seq-len ,bound-index)
                        (multiple-value-bind (,updated-result ,next-index ,success)
                            ,(match-sub-vector bound-seq bound-seq-len
                                               nil bound-index matched-seq)
                          (if ,success
                              ;; reuse a previous result
                              (values ,matched-result ,next-index ,success)
                              (values ,updated-result ,bound-index ,success)))))
                 (declare (inline ,bind-sym))
                 ,(sequence-expressions seq seq-length updated-result
                                        next-index exprs))))))))


;; Optional:
;; PEG: expr? => (? expr)
(defun optional-expression (seq seq-length current-result current-index
                            expr)
  (let ((next-index     (gensym))
       	(updated-result (gensym))
        (success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
	 ,(translate-expression seq seq-length current-result current-index
                                expr)
       (if ,success
	   (values ,updated-result
		   ,next-index
		   ,success)
	   (values (cons nil ,current-result)
		   ,current-index
		   t)))))


;; Zero-or-more:
;; PEG: expr* => (repeat expr...) (Note: implicit sequence)
(defun repeat-of-an-expression (seq seq-length current-result current-index exprs)
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
           ,(translate-expression seq seq-length result index exprs)
         (declare (type fixnum ,next-index))
         (if (and ,success (<= ,next-index ,seq-length))
             (progn
               (setf ,result ,updated-result)
               (setf ,index ,next-index))
             (return (values ,result ,index t)))))))


;; Ordered choice:
;; PEG: expr1 / expr2 / ... => (/ expr1 expr2 ...)
(defun ordered-choice-expressions (seq seq-length current-result current-index exprs)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(block nil
       ,(reduce (lambda (translated-expression failure-continuation)
                  `(multiple-value-bind (,updated-result ,next-index ,success)
                       ,translated-expression
                     (if ,success
                         (return (values ,updated-result ,next-index ,success))
                         ,failure-continuation)))
                (mapcar (lambda (expression)
                          (translate-expression seq
                                                seq-length
                                                current-result
                                                current-index
                                                expression))
                        exprs)
                :from-end t
                :initial-value `(values ,updated-result ,current-index ,success)))))


;; "And" predicate:
;; PRG: &expr => (and expr)
(defun and-predicate (seq seq-length current-result current-index expr)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
         ,(translate-expression seq
                                seq-length
				current-result
				current-index
				expr)
       (declare (ignore ,next-index))
       (values (if ,success
                   ,current-result
                   ,updated-result)
               ,current-index
               ,success))))


;; "Not" predicate:
;; PRG: !expr => (not expr)
(defun not-predicate (seq seq-length current-result current-index expr)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
         ,(translate-expression seq
                                seq-length
				current-result
				current-index
				expr)
       (declare (ignore ,next-index))
       (values (if ,success
                   ,current-result
                   ,updated-result)
               ,current-index
               (not ,success)))))


;; Just skip matched sequence
(defun skip-if-match (seq seq-length current-result current-index exprs)
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    `(multiple-value-bind (,updated-result ,next-index ,success)
         ,(sequence-expressions seq
                                seq-length
				current-result
				current-index
				exprs)
      (if ,success
          (values ,current-result
                  ,next-index
                  ,success)
          (values ,updated-result
                  ,current-index
                  ,success)))))

;; Three predicate forms for terminal symbols
;; 1: '(predicate-call) -> (predicate-call x)
(defun predicate-call (seq seq-length current-result current-index predicate-call)
  `(if (and (< ,current-index ,seq-length)
	    (,@predicate-call (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index) ,current-result)
	       (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
	       t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail ',predicate-call)
               ,current-index
               nil)))

;; 2: 'symbol == '(eq 'symbol x)
(defun predicate-eq (seq seq-length current-result current-index symbol)
  `(if (and (< ,current-index ,seq-length)
              (eq (quote ,symbol) (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
	       (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
	       t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail '(eq ,symbol))
	       ,current-index
	       nil)))

;; 3: literal == '(eql literal x)
(defun predicate-eql (seq seq-length current-result current-index literal)
  `(if (and (< ,current-index ,seq-length)
	    (eql ,literal (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
               (locally (declare (optimize (safety 0)))
                 (the fixnum (1+ ,current-index)))
	       t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail '(eql ,literal))
	       ,current-index
	       nil)))

;; 4: 'literal == '(equal literal x)
(defun predicate-equal (seq seq-length current-result current-index literal)
  `(if (and (< ,current-index ,seq-length)
	    (equal ,literal (elt ,seq ,current-index)))
       (values (cons (elt ,seq ,current-index)
		     ,current-result)
               (locally (declare (optimize (safety 0)))
                 (the fixnum (1+ ,current-index)))
	       t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail '(equal ,literal))
	       ,current-index
	       nil)))

(defun predicate-expression (seq seq-length current-result current-index expr)
  (cond
    ((symbolp expr)
     (predicate-eq    seq seq-length current-result current-index expr))
    ((consp expr)
     (predicate-call  seq seq-length current-result current-index expr))
    (t
     (predicate-equal seq seq-length current-result current-index expr))))


;; "*": always success with consuming a single input element
(defun any-element (seq seq-length current-result current-index)
  `(if (< ,current-index ,seq-length)
       (values (cons (elt ,seq ,current-index) ,current-result)
               (locally (declare (optimize (safety 0)))
		 (the fixnum (1+ ,current-index)))
               t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail 'unexpected-end-of-input)
	       ,current-index
	       nil)))


;; "nil": always success without consuming the input sequence
(defun empty-element (seq seq-length current-result current-index)
  (declare (ignore seq seq-length))
  `(values ,current-result ,current-index t))


;; $: success if the current index is out or range
(defun end-element (seq seq-length current-result current-index)
  (declare (ignorable seq))
  `(if (>= ,current-index ,seq-length)
       (values ,current-result
               ,current-index
               t)
       (values (peg::make-peg-failure-descriptor
                :pos ,current-index :fail 'unterminated)
	       ,current-index
	       nil)))


;; parser: call another parser
(defun call-external-parser (seq seq-length current-result current-index parser)
  (declare (ignorable seq-length))
  (let ((updated-result (gensym))
	(next-index     (gensym))
	(success        (gensym)))
    ;; use a given parser for a partial match
    ;; (ignore exhausted-p of retvalues)
    `(multiple-value-bind (,updated-result ,next-index ,success)
	 (,parser ,seq :start-index ,current-index)
       (if ,success
           (values (cons ,updated-result ,current-result)
                   (locally
                       ;; stop warning about potentially failing coercion
                       (declare #+sbcl(sb-ext:muffle-conditions
                                       sb-ext:compiler-note))
                     (coerce ,next-index 'fixnum))
                   t)
	   (values ,updated-result
		   ,current-index
		   ,success)))))


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
(defun match-sub-vector (seq seq-length current-result current-index sub-vector)
  (let ((i             (gensym))
        (j             (gensym))
        (target        (gensym))
        (vector-length (gensym)))
    `(let ((,target ,sub-vector)
           (,vector-length ,(if (subtypep (type-of sub-vector) 'sequence)
                                (length sub-vector)
                                `(length ,sub-vector))))
       (if (>= (- ,seq-length ,current-index) ,vector-length)
           (do ((,i ,current-index (1+ ,i))
                (,j 0              (1+ ,j)))
               ((= ,j ,vector-length)
                (values (cons (subseq ,seq ,current-index ,i)
                              ,current-result)
                        ,i
                        t))
             (if (not (eql (elt ,seq ,i) (elt ,target ,j)))
                 (return (values (peg::make-peg-failure-descriptor
                                  :pos    ,current-index
                                  :fail (list 'start-with ,sub-vector))
                                 ,current-index
                                 nil))))
           (values (peg::make-peg-failure-descriptor
                    :pos    ,current-index
                    :fail (list 'start-with ,sub-vector))
                   ,current-index ,nil)))))


(defun translate-expression (seq seq-length current-result current-index expr)
  (cond
    ;; Use string comparisons (instead of symbol eqs) to
    ;; ignore namespaces.
    ;; These comparisons run only at compilation time, so
    ;; there are no performance penalty at runtime.
    ((symbolp expr)
     (cond
       ;; '*' matches to any symbols
       ((equal (symbol-name expr) "*")
        (any-element   seq seq-length current-result current-index))
       
       ;; 'nil' matches to an 'empty' input
       ((eq expr nil)
        (empty-element seq seq-length current-result current-index))

       ;; '$' matches to the out of the input
       ((equal (symbol-name expr) "$")
        (end-element   seq seq-length current-result current-index))
       
       ;; match to a non-terminal symbol
       (t
        (let ((match-result (gensym))
	      (next-index   (gensym))
	      (success      (gensym)))
          `(multiple-value-bind (,match-result ,next-index ,success)
               (,expr ,seq ,seq-length ,current-index)
             (declare (type fixnum ,next-index))
             (values (if ,success
                         (cons ,match-result ,current-result)
                         ,match-result)
                     ,next-index
                     ,success))))))
    
    ;; match a given expression to one of the PEG syntactic rules
    ((consp expr)
     (let ((head (car expr)))
       (if (symbolp head)
           (let ((head-name (symbol-name head)))
             (cond
               ;; Expressions that take a single sub-expression
               ((equal head-name "QUOTE")
                (predicate-expression       seq seq-length current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "PARSER")
                (call-external-parser       seq seq-length current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "?")
                (optional-expression        seq seq-length current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "&")
                (and-predicate              seq seq-length current-result current-index
                                            (cadr expr)))
               
               ((equal head-name "!")
                (not-predicate              seq seq-length current-result current-index
                                            (cadr expr)))
               
               ;; Expressions that take multiple sub-expressions
               ((equal head-name "REPEAT")
                (repeat-of-an-expression    seq seq-length current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "_")
                (skip-if-match              seq seq-length current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "/")
                (ordered-choice-expressions seq seq-length current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "GROUP")
                (group-expressions          seq seq-length current-result current-index
                                            (cdr expr)))
               
               ((equal head-name "LET")
                (let-expression             seq seq-length current-result current-index
                                            (cdr expr)))
               
               ;; Sequence expression is a default option
               (t
                (sequence-expressions       seq seq-length current-result current-index
                                            expr))))
           
           ;; A sequence expression of which head is not a symbol (literal ...)
           (sequence-expressions      seq seq-length current-result current-index
                                      expr))))

    ;; A sub-vector expression
    ((vectorp expr)
     (match-sub-vector     seq seq-length current-result current-index
                           expr))
    
    ;; Otherwise, assume this is a scalar literal
    (t
     (predicate-eql       seq seq-length current-result current-index
                          expr))))

(defstruct memoise-value
  (result nil :type t)
  (index 0    :type fixnum)
  (state nil  :type boolean))

(defun translate-single-rule (nt-symbol rule-body memo result-interpretor)
  (let ((seq            (gensym))
        (seq-length     (gensym))
        (updated-result (gensym))
        (current-index  (gensym))
        (next-index     (gensym))
	(success        (gensym))
        (lookup-result  (gensym))
        (lookup-success (gensym)))
    `(,nt-symbol (,seq ,seq-length ,current-index)
                 (declare (ignorable ,seq-length)
                          (type (or simple-vector simple-string) ,seq)
                          (type fixnum ,current-index ,seq-length))
		 ;; (format t "~A: ~A...~&" (symbol-name ',nt-symbol)
		 ;; 	 (subseq ,seq ,current-index
		 ;; 		 (min (+ ,current-index 20) ,seq-length)))
                 (multiple-value-bind (,lookup-result ,lookup-success)
                     (gethash ,current-index ,memo)
                   (if ,lookup-success
                       (locally
                           (declare (type memoise-value ,lookup-result))
                         (values (memoise-value-result ,lookup-result)
                                 (memoise-value-index  ,lookup-result)
                                 (memoise-value-state  ,lookup-result)))
                       (progn
                         ;; add a sentinel to reject reentrant evaluations
                         ;; (maybe left recursions?)
                         (setf (gethash ,current-index ,memo)
                               (peg::make-memoise-value
                                :result (peg::make-peg-failure-descriptor
                                         :pos ,current-index :fail 'left-recursion)
                                :index  ,current-index
                                :state  nil))
                         (multiple-value-bind (,updated-result ,next-index ,success)
                             ,(translate-expression seq
                                                    seq-length
                                                    nil
                                                    current-index
                                                    rule-body)
                           (declare (type fixnum ,next-index) (type boolean ,success))
                           (let ((,updated-result (if ,success
                                                      (apply ,result-interpretor
                                                             (nreverse ,updated-result))
                                                      ,updated-result)))
                             (let ((,lookup-result (nth-value 0 (gethash ,current-index ,memo))))
                               (declare (type memoise-value ,lookup-result))
                               (setf (memoise-value-result ,lookup-result) ,updated-result
                                     (memoise-value-index ,lookup-result) ,next-index
                                     (memoise-value-state ,lookup-result) ,success)
			       ;; (format t "~A: -> ~A (~A)~&" (symbol-name ',nt-symbol)
			       ;; 	       ,updated-result ,success)
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


(defparameter *optimization-flag* '((speed 3)))

(defun default-interpretor (&rest xs)
  (if (and xs (car xs) (not (peg-failure-descriptor-p (car xs))))
      t
      nil))

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
        (memoise-tables (make-hash-table)))
    (let ((inlinable-symbols (collect-inlinable start-symbols
                                                non-terminal-symbols
                                                rule-bodies)))
      (mapc (lambda (non-terminal)
              (setf (gethash non-terminal memoise-tables)
                    (gensym)))
            non-terminal-symbols)
      `(labels ((,process (,input-sequence ,start-index)
                  (declare (type (or simple-vector simple-string) ,input-sequence)
                           (type fixnum ,start-index)
                           (optimize ,@dtakahashi.peg:*optimization-flag*)
                           #+sbcl(sb-ext:muffle-conditions
                                  sb-ext:compiler-note))
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
                                                                  '#'dtakahashi.peg::default-interpretor)))
                                     non-terminal-symbols
                                     rule-bodies
                                     result-interpretors)
                      (declare ,@(if inlinable-symbols `((inline ,@inlinable-symbols))))
                      (let ((,input-length (length ,input-sequence)))
                        (or
                         ,@(mapcar (lambda (start-symbol)
                                     `(multiple-value-bind (,result ,index-next ,success)
                                          (,start-symbol ,input-sequence ,input-length ,start-index)
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
                                   (if (or (typep input-sequence 'simple-vector)
                                           (typep input-sequence 'simple-string))
                                       input-sequence
                                       (coerce input-sequence 'simple-vector)))))
             (,process input-sequence start-index)))))))
