#lang racket

;; Define Either (Result) type 
(struct success (value) #:transparent)
(struct failure (message) #:transparent)

;; Helper functions for Either type
(define (from-success v x)
  (if (success? x)
      (success-value x)
      v))

(define (from-failure v x)
  (if (failure? x)
      (failure-message x)
      v))

;; Function to check if an item is in a list
(define in-list?
  (λ (x lst)
    (not (false? (member x lst)))))

;; Safe division that returns Either
(define (safe-div x y)
  (if (= y 0)
      (failure "Division by zero")
      (success (/ x y))))

;; Check if a string is a valid identifier
(define (valid-id? id-str)
  (and (string? id-str)
       (> (string-length id-str) 0)
       (char-alphabetic? (string-ref id-str 0))
       (for/and ([c (in-string id-str)])
         (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\_)
             (char=? c #\-)))))

;; Convert symbol to string for ID validation
(define (valid-id-sym? id-sym)
  (valid-id? (symbol->string id-sym)))

;; Get a variable's value from state
(define (get-var id state)
  (let ([var-pair (assoc id state)])
    (if var-pair
        (let ([val (second var-pair)])
          (if (eq? val 'undefined)
              (failure (format "Variable '~a' is undefined" id))
              (success val)))
        (failure (format "Variable '~a' not found" id)))))

;; Evaluate expressions with state
(define (eval expr state)
  (cond
    ;; Number literal
    [(equal? (first expr) 'num) 
     (values (success (second expr)) state)]
    
    ;; Variable reference
    [(equal? (first expr) 'id)
     (let ([id (second expr)])
       (let ([result (get-var id state)])
         (values result state)))]
    
    ;; Arithmetic operations
    [(in-list? (first expr) '(div add sub mult))
     (let-values ([(x-result x-state) (eval (second expr) state)])
       (if (failure? x-result)
           (values x-result state) ; Return the failure
           (let-values ([(y-result y-state) (eval (third expr) x-state)])
             (if (failure? y-result)
                 (values y-result state) ; Return the failure
                 (let ([x-val (success-value x-result)]
                       [y-val (success-value y-result)])
                   (case (first expr)
                     [(div) (values (safe-div x-val y-val) y-state)]
                     [(add) (values (success (+ x-val y-val)) y-state)]
                     [(sub) (values (success (- x-val y-val)) y-state)]
                     [(mult) (values (success (* x-val y-val)) y-state)]))))))]
    
    ;; Define variable
    [(equal? (first expr) 'define)
     (let ([id (second expr)])
       (if (not (valid-id-sym? id))
           (values (failure (format "Invalid identifier: ~a" id)) state)
           (if (assoc id state)
               (values (failure (format "Variable '~a' already defined" id)) state)
               (if (= (length expr) 2) ; (define id) form
                   (values (success 'undefined) (cons (list id 'undefined) state))
                   (let-values ([(val-result val-state) (eval (third expr) state)])
                     (if (failure? val-result)
                         (values val-result state)
                         (values (success (success-value val-result)) 
                                 (cons (list id (success-value val-result)) val-state))))))))]
    
    ;; Assign to variable
    [(equal? (first expr) 'assign)
     (let ([id (second expr)])
       (if (not (assoc id state))
           (values (failure (format "Cannot assign to undefined variable '~a'" id)) state)
           (let-values ([(val-result val-state) (eval (third expr) state)])
             (if (failure? val-result)
                 (values val-result state)
                 (let ([new-val (success-value val-result)])
                   (values (success new-val)
                           (map (lambda (var-pair)
                                  (if (equal? (first var-pair) id)
                                      (list id new-val)
                                      var-pair))
                                val-state)))))))]
    
    ;; Remove variable
    [(equal? (first expr) 'remove)
     (let ([id (second expr)])
       (if (not (assoc id state))
           (begin
             (printf "Error: remove ~a: variable not defined, ignoring~n" id)
             (values (success 'removed) state))
           (values (success 'removed)
                   (filter (lambda (var-pair) (not (equal? (first var-pair) id))) state))))]
    
    ;; Unknown operation
    [else (values (failure (format "Unknown operation: ~a" (first expr))) state)]))

;; REPL loop
(define (repl-loop)
  (let loop ([state '()])
    (printf "expr> ")
    (let ([input (read)])
      (cond
        [(eq? input 'quit) 
         (printf "Goodbye!~n")]
        [(eq? input 'state)
         (printf "Current state: ~a~n" state)
         (loop state)]
        [else
         (let-values ([(result new-state) (eval input state)])
           (if (success? result)
               (printf "Success: ~a~nState: ~a~n" (success-value result) new-state)
               (printf "Failure: ~a~nState: ~a~n" (failure-message result) state))
           (loop new-state))]))))

;; Test expressions
(define (run-tests)
  (let-values ([(r1 s1) (eval '(num 5) '())])
    (printf "Test 1: ~a~n" r1))
  
  (let-values ([(r2 s2) (eval '(add (num 5) (mult (num 2) (num 3))) '())])
    (printf "Test 2: ~a~n" r2))
  
  (let-values ([(r3 s3) (eval '(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10))(num 6))) '())])
    (printf "Test 3: ~a~n" r3))
  
  (let-values ([(r4 s4) (eval '(div (num 5) (sub (num 5) (num 5))) '())])
    (printf "Test 4: ~a~n" r4))
  
  (let* ([init-state '()]
         [s1 (let-values ([(r s) (eval '(define a (num 5)) init-state)]) s)]
         [s2 (let-values ([(r s) (eval '(define b (add (id a) (num 3))) s1)]) s)]
         [s3 (let-values ([(r s) (eval '(assign a (add (id a) (num 1))) s2)]) s)])
    (let-values ([(r s) (eval '(add (id a) (id b)) s3)])
      (printf "Variable test: ~a~nFinal state: ~a~n" r s))))

;; Start REPL
(printf "Simple Expression Evaluator~n")
(printf "Type 'quit' to exit, 'state' to see current state~n")
(repl-loop)
