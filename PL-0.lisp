
(defun translate-program-block (cnst var proc stmt)
  `(let-const ,cnst
              (let-var ,var
                       (let-proc ,proc
                                 ,stmt))))
(defun translate-assign (id expl) `(setf ,id ,expl))
(defun translate-stmt-block (&rest xs) (cons 'progn xs))
(defun translate-if (pred stmt) `(if ,pred ,stmt))
(defun translate-while (pred stmt) `(while ,pred ,stmt))
(defun translate-odd-p (expr) `(oddp ,expr))
(defun translate-comparison (expr1 cmp expr2)
  (cond
    ((equal cmp "=") `(= ,expr1 ,expr2))
    ((equal cmp "#") `(not (= ,expr1 ,expr2)))
    ((equal cmp "<") `(< ,expr1 ,expr2))
    ((equal cmp "<=") `(<= ,expr1 ,expr2))
    ((equal cmp ">") `(> ,expr1 ,expr2))
    ((equal cmp ">=") `(>= ,expr1 ,expr2))))
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
                        `(* ,(cadr x))))
                  factors)
          :initial-value factor))
(defun intern-symbol (x) (intern (concatenate 'string x)))
(defun read-number (x) (read-from-string (concatenate 'string x)))

(peg:defparser pl0 (<block>)
  (<block>      ((? <const>) (? <var>) (group (repeat <procedure>)) <statement>)
                #'translate-program-block)
  (<const>      ((group (_ <> "const") <ident> (_ "=") <num>)
                 (group (repeat <const-rest>)) (_ ";"))
                #'cons)
  (<const-rest> ((_ ",") <ident> (_ "=") <num>)
                #'list)
  (<var>        ((_ <> "var") <ident> (repeat <var-rest> (_ ";")))
                #'list)
  (<var-rest>   ((_ ",") <ident>)
                #'identity)
  (<procedure>  ((_ <> "procedure") <ident> (_ ";") <block> (_ ";"))
                #'list)
  (<statement>  ((_ <>) (/ <assign> <call> <stmt-block> <if> <while>))
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

;; CL-USER> (pl0 "
;; const c = 12, d=2;
;; var x, y;
;; begin
;;   x := 20;
;;   y := 0;
;;   while x > 0 do
;;     if odd x then
;;       begin
;;         x := x - 1;
;;         y := y + x
;;       end
;; end")
;; (LET-CONST ((|c| 12) (|d| 2))
;;  (LET-VAR (|x| |y|)
;;   (LET-PROC NIL
;;    (PROGN
;;     (SETF |x| 20)
;;     (SETF |y| 0)
;;     (WHILE (> |x| 0)
;;      (IF (ODDP |x|)
;;          (PROGN (SETF |x| (- |x| 1)) (SETF |y| (+ |y| |x|)))))))))
;; 156
;; T
;; T
