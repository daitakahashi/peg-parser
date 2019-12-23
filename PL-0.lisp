
(defun translate-program-block (consts vars procs stmt)
  `(let ,(append consts vars)
       (labels ,procs
	 ,stmt)))
(defun translate-procedure (id block)
  `(,id () ,block))
(defun translate-assign (id expl) `(setf ,id ,expl))
(defun translate-stmt-block (&rest xs) (cons 'progn xs))
(defun translate-if (pred stmt) `(if ,pred ,stmt))
(defun translate-while (pred stmt) `(do () ((not ,pred)) ,stmt))
(defun translate-odd-p (expr) `(oddp ,expr))
(defun translate-comparison (expr1 cmp expr2)
  (cond
    ((equal cmp "=") `(= ,expr1 ,expr2))
    ((equal cmp "#") `(not (= ,expr1 ,expr2)))
    ((equal cmp "<") `(< ,expr1 ,expr2))
    ((equal cmp "<=") `(<= ,expr1 ,expr2))
    ((equal cmp ">") `(> ,expr1 ,expr2))
    ((equal cmp ">=") `(>= ,expr1 ,expr2))))
(defun translate-write (expr) `(format t "~A~&" ,expr))
(defun translate-expr (sign term &rest terms)
  (reduce (lambda (x y) `(,(car y) ,x ,(cadr y)))
          (mapcar (lambda (x)
                    (if (equal (car x) "+")
                        `(+ ,(cadr x))
                        `(- ,(cadr x))))
                  terms)
          :initial-value (if (equal sign "-") `(- ,term) term)))
(defun translate-term (factor &rest factors)
  (reduce (lambda (x y) `(,(car y) ,x ,(cadr y)))
          (mapcar (lambda (x)
                    (if (equal (car x) "*")
                        `(* ,(cadr x))
                        `(floor ,(cadr x)))) ;; integer division
                  factors)
          :initial-value factor))
(defun intern-symbol (x) (intern (concatenate 'string x)))
(defun read-number (x) (read-from-string (concatenate 'string x)))

(peg:defparser pl0 (<block>)
  (<block>      ((_ <>) (? <const>)
		 (_ <>) (? <var>)
		 (_ <>) (group (repeat <procedure> (_ <>)))
		 <statement> (_ <>))
                #'translate-program-block)
  (<const>      ((group (_ <> "const") <ident> (_ "=") <num>)
                 (group (repeat <const-rest>)) (_ ";"))
                #'cons)
  (<const-rest> ((_ ",") <ident> (_ "=") <num>)
                #'list)
  (<var>        ((_ <> "var") <ident> (repeat <var-rest>) (_ ";"))
                #'list)
  (<var-rest>   ((_ ",") <ident>)
                #'identity)
  (<procedure>  ((_ "procedure") <ident> (_ ";") <block> (_ ";"))
                #'translate-procedure)
  (<statement>  ((_ <>) (/ <assign> <call> <stmt-block> <if> <while> <write>))
                #'identity)
  (<assign>     (<ident> (_ ":=") <expression>)
                #'translate-assign)
  (<call>       ((_ "call") <ident>)
                #'list)
  (<stmt-block> ((_ "begin") <statement> (repeat (_ <> ";") <statement>) (_ <> "end"))
                #'translate-stmt-block)
  (<if>         ((_ "if") <condition> (_ "then") <statement>)
                #'translate-if)
  (<while>      ((_ "while") <condition> (_ "do") <statement>)
                #'translate-while)
  (<condition>  ((_ <>) (/ <odd-p> <comparison>))
                #'identity)
  (<odd-p>      ((_ "odd") <expression>)
                #'translate-odd-p)
  (<comparison> (<expression> (/ "=" "#" "<" "<=" ">" ">=") <expression>)
                #'translate-comparison)
  (<write>      ((_ (/ "!" "write")) <expression>)
		#'translate-write)
  (<expression> ((? (/ "+" "-")) <term> (repeat <expr-rest>))
                #'translate-expr)
  (<expr-rest>  ((_ <>) (/ "+" "-") (_ <>) <term>)
                #'list)
  (<term>       (<factor> (repeat <term-rest>))
                #'translate-term)
  (<term-rest>  ((_ <>) (/ "*" "/") (_ <>) <factor>)
                #'list)
  (<factor>     (/ <ident> <num> ((_ "(" <>) <expression> (_ <> ")")))
                #'identity)
  (<ident>      ((_ <>) (group '(alpha-char-p) (repeat '(alphanumericp))) (_ <>))
                #'intern-symbol)
  (<num>        ((_ <>) (group '(digit-char-p) (repeat '(digit-char-p))) (_ <>))
                #'read-number)
  (<>           (repeat (/ #\Space #\Tab #\Newline))))


;; example from Wikipeda: prime numbers upto 100
;; (eval (pl0 "
;; const max = 100;
;; var arg, ret;
;;
;; procedure isprime;
;; var i;
;; begin
;; 	ret := 1;
;; 	i := 2;
;; 	while i < arg do
;; 	begin
;; 		if arg / i * i = arg then
;; 		begin
;; 			ret := 0;
;; 			i := arg
;; 		end;
;; 		i := i + 1
;; 	end
;; end;
;;
;; procedure primes;
;; begin
;; 	arg := 2;
;; 	while arg < max do
;; 	begin
;; 		call isprime;
;; 		if ret = 1 then write arg;
;; 		arg := arg + 1
;; 	end
;; end;
;;
;; call primes    
;; "))
