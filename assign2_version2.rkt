#lang racket

(define stack null)
(define stackFunc
        (lambda (command . args)
            (case command
                ((disp) (displayln stack))
                ((empty?) (null? stack))
                ((top) 
                  (if (null? stack)
                    ""
                    (car stack))
                  )
                ((push) 
                  (set! stack (cons (car args) stack))
                )
                
                ((removeTop) 
                  (if (null? stack)
                    ""
                    (set! stack (cdr stack)))
                )

                ((pop)
                 
                 (begin
                    (if (null? stack)
                      ""
                      (let ((value (car stack)))
                        (set! stack (cdr stack))
                         value)
                      )
                    ))
                (else 'error))))

(define registerCounter 1)  ; define a register Counter

(define comp
  (lambda (var)
      (begin
        (if (list? var)

          (if (eqv? 3 (length var))
            (let* ((op (list-ref var 0))
              (operand1 (list-ref var 1))
              (operand2 (list-ref var 2)))
              (begin
                (if (number? operand1)
                  (begin
                    (display "move ") (display operand1) 
                    (display " register- ") (displayln registerCounter)
                    (stackFunc `push registerCounter)  ; save register counter in stack
                    (set! registerCounter (+ 1 registerCounter)))
                  (comp operand1);else
                  )
                (if (number? operand2)
                  (begin
                    (display "move ") (display operand2) 
                    (display " register- ") (displayln registerCounter)
                    (stackFunc `push registerCounter)  ; save register counter in stack
                    (set! registerCounter (+ 1 registerCounter)))
                  ;else
                  (comp operand2)
                  )
                (if (symbol? op)
                  (begin
                    (cond
                      [(eqv? op `+)
                          (display "add ")] 
                      [(eqv? op `-)
                          (display "substrate ")] 
                      [(eqv? op `*)
                          (display "times ")] 
                      [(eqv? op `/)
                          (display "divides ")]
                      [else (displayln "***** expr is not correct ****") 
                      ]
                    ) 
                    (let* ((re2 (stackFunc `pop)) (re1 (stackFunc `top)))
                      (begin
                        (display "register -") 
                        (display re1) 
                        (display "  register -") 
                        (displayln re2) 
                        ))
                    
                    )
                  (displayln "****** Syntax error #2 *******") ; else
                  )
              )
            );end let
            (begin
              (display var)
              (if (eqv? 2 (length var))
                (displayln "\n***** missing operand *****")
                (displayln "\n***** unknown operand *****" ))))  ; have more than 2 operand)
          ;else statement
          (begin ; if the var is not list, error

            (displayln "****** Syntax error #1 *******")
            (displayln "****** Could lead by wrong operand passing *******")
          )))))


;main
(define main
  (lambda (ope)
    (begin
      (set! registerCounter 1)
      (comp ope)
      (stackFunc `removeTop)
      (if (null? stack)
        ""
        (displayln "there is something in stack\n syntax error")))))

(define compile
  (lambda ()
    (begin
      (displayln "\nInput: ") 
      (main (read))
      (compile))))

; testCase case 
(begin

  ;test function 
  (define (test testcase testls)
    (begin
      (display testcase)
      (displayln ": ")
      (displayln testls)

      (main testls)
      ; (set! registerCounter 1)
      ; (comp testls)
      ; (stackFunc `removeTop)

      (display "Finish operation, Stack size is: ")  
      (displayln (length stack)) 

      (newline)      
    )
  )



  (define testCase1  `(+ 2 3))
  (define testCase2  `(- 2 3))
  (define testCase3  `(* 2 3))
  (define testCase4  `(/ 2 3))

  (define testCase11  `(+ 2 (+ 2 3)))
  (define testCase12  `(- 2 (+ 2 3)))
  (define testCase13  `(* 2 (+ 2 3)))
  (define testCase14  `(/ 2 (+ 2 3)))

  (define testCase21  `(/ (+ 1 2) (+ 3 3))) 
  (define testCase22  `(* (+ 5 5) (/ (/ 3 3) (/ 1 1))))

  (define testCase31  `(1 1 2))
  (define testCase32  `(* * 2))
  (define testCase33  `(* 1))
  (define testCase34  `(* 9 ?))
  (define testCase35  `(& (+ 5 5) 8 9))
  ; (test testCase1)
  ; (test testCase2)
  ; (test testCase3)
  ; (test testCase4)


  (displayln "=============== Regular operation test: =============") 
  (test `testCase11 testCase11)
  (test `testCase12 testCase12)
  (test `testCase13 testCase13)
  (test `testCase14 testCase14)

  (test `testCase21 testCase21)
  (test `testCase22 testCase22)

  (displayln "=============== Wrong operation test: =============") 
  (test `testCase31 testCase31)
  (test `testCase32 testCase32)
  (test `testCase33 testCase33)
  (test `testCase34 testCase34)
  (test `testCase35 testCase35)
  )


; (compile)