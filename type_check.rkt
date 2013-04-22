(define blopp
  (lambda ()
    (let ((a (read)))
      (secondss a)
      (blopp))))

(define show
  (lambda (type arg)
    (begin
      (display type)
      (display ": ")
      (display arg)
      (newline))))

(define secondss
  (lambda (var)
      (cond ((number? var) (show "Number" var))
            ((char? var) (show "Character" var))
            ((string? var) (show "String" var))
            ((symbol? var) (show "Symbol" var))
            ((list? var) 
              ;(show "List" var)
              (displayln (car var))
              (let* ((temp (cdr var)))
                (if (null? (cdr var)) (display "")
                  (secondss temp)
                )
                 ))
            (else (show "Unknown type" var)))
      ))

(secondss "asdf")
(secondss 5)
(secondss `(* 5 5))
(secondss `(* (* 5 6) (* 7 8)))
(blopp)