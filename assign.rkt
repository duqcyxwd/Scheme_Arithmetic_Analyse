(require racket/string) 

; stack
(define make-stack
    (lambda (stack)
        (lambda (command . args)
            (case command
                ((disp) (displayln stack))
                ((empty?) (null? stack))
                ((top) (car stack))
                ((push!) 
                    (set! stack (cons (car args) stack))
                    (car stack))
                ((remove-top!) 
                    (set! stack (cdr stack)))
                ((pop!) (let ((value (car stack)))
                    (set! stack (cdr stack))
                    value))
                (else 'error)))))
(define (pushFromList ls stack)
    (if (null? ls) ""
        (begin
            (stack 'push! (car ls))
            (pushFromList (cdr ls) stack))))


;display the list
(define (show ls)
    (if (null? ls) (display "")
        (begin
            (display (car ls))
            (display " ")
            (show (cdr ls)))))


;program to slove the problem
(define registerCounter 0)
(define register-stack (make-stack '()))
(define (ope stack)
    (if (stack 'empty?) (display "")
        (begin
            (let* ((topChar (stack 'pop!)))
                (cond
                    [(or (string=? topChar "(") (string=? topChar  ")"))
                        (ope stack)
                    ]
                    [(or (string=? topChar "+") (string=? topChar  "-") (string=? topChar  "*") (string=? topChar  "/"))
                        ; (display "+")
                        (define op `*)
                        (cond
                            [(string=? topChar "+")
                                (set! op +)
                                (display "add ")
                                ] 
                            [(string=? topChar "-")
                                (set! op -)
                                (display "substrate ")
                                ] 
                            [(string=? topChar "*")
                                (set! op *)
                                (display "times ")
                                ] 
                            [(string=? topChar "/")
                                (set! op /)
                                (display "divides ")
                                ]
                        )
                        (let ((var (op (ope stack) (ope stack))))
                            (let ((reg1 (register-stack `pop!))  (reg2 (register-stack `top)) )
                                (begin
                                    (display "register-") 
                                    (display reg2)
                                    (display " register-")
                                    (displayln reg1)))
                            var)]
                    [else 
                        (set! registerCounter (+ registerCounter 1))
                        (display "move ")
                        (display topChar)
                        (display " register-")
                        (displayln registerCounter)

                        ;TODO- add the the type check
                        (register-stack `push! registerCounter)
                        (string->number topChar)]
            ))
        )))

;main program , read input from user
(define (main)
    (let ((st (read-line)))
        (if (not(eof-object? st))
            (begin
                ;(display (symbol->string st))
                (newline)
                (let ((value (compile0 (string-split st))))
                    (display "result: ")
                    (displayln value))
                (main)))))

(define (compile0 testls)
    (set! registerCounter 0)
    (define stack1 (make-stack '())); define a stack for test list, 
    (pushFromList (reverse testls) stack1)
    (ope stack1))



; test case
(begin 
    (define counter 0) ;define a counter for test
    ;test function 
    (define (test testls)
        ; (define stack1 (make-stack '()))
        ; (pushFromList (reverse testls) stack1)

        (set! counter (+ counter 1))
        (display "test ")
        (display counter)
        (display ": ")
        (show testls)
        (newline)

        (let ((value (compile0 testls)))
            (display "result: ")
            (displayln value)
        )
        (newline)        
    )

    (define test1  (string-split  "( + 2 3 )" ) )
    (define test2  (string-split  "( - 2 3 )" ) )
    (define test3  (string-split  "( * 2 3 )" ) )
    (define test4  (string-split  "( / 2 3 )" ) )

    (define test11  (string-split  "( + 2 ( + 2 3 ) )" ) )
    (define test12  (string-split  "( - 2 ( + 2 3 ) )" ) )
    (define test13  (string-split  "( * 2 ( + 2 3 ) )" ) )
    (define test14  (string-split  "( / 2 ( + 2 3 ) )" ) )

    (define test21  (string-split  "( / ( + 1 2 ) ( + 3 3 ) )" ) )
    (define test22  (string-split  "( * ( + 5 5 ) ( / ( / 3 3 ) ( / 1 1 ) ) )" ) )  

    ; (test test1)
    ; (test test2)
    ; (test test3)
    ; (test test4)

    (test test11)
    (test test12)
    (test test13)
    (test test14)

    (test test21)
    (test test22)
)

(main)
