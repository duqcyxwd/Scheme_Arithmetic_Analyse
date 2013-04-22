(define  (celsius->fahrenheit  celsius)
	(+  (*  celsius  1.8)  32))
;;(celsius->fahrenheit  0)
;;(celsius->fahrenheit  100)
;;(celsius->fahrenheit  -40)

;;(load "scheme-examples.scm")

(define (leap-year? year)
  (or (and (zero? (remainder year 4))
           (not (zero? (remainder year 100))))
      (zero? (remainder year 400))))

(leap-year? 2000)

;;
; Function: sum
; -------------
; Computes the sum of all of the numbers in the specified
; number list.  If the list is empty, then the sum is 0.
; Otherwise, the sum is equal to the value of the car plus
; the sum of whatever the cdr holds.
;;
(define (sum ls)
  (if (null? ls) 0
      (+ (car ls) (sum (cdr ls)))))

;;(sum  '(4.5  2.7  3.2  0.7))

(define max-mag
  (lambda nums
    (apply max (map magnitude nums))))
;;(max 1 -2 0 2 3 4)


;; if .....
(define a (read))
    (define b (read))
 
    ; 输出结果，包含从数字到字符串的转化
    (define (display_rs x)
        (display (string-append (number->string a) x (number->string b)))
    )
 
    (if (< a b) 
        (display_rs "<")
        (if (= a b)
            (display_rs "=")
            (display_rs ">")
        )
    )
 
(newline)	



; stack
	(lambda (stack)
		(lambda (command . args)
			(case command
				((empty?) (null? stack))
				((top) (car stack))
				((push!) 
					(set! stack (cons (car args) stack))
					(car stack)
				)
				((pop!) (let ((value (car stack)))
					(set! stack (cdr stack))
					value)
				)
				(else 'error)
			)
		)
	)
)


; > (define stack1 (make-stack '()))
; > (stack1 'empty?)
; #t
; > (stack1 'push! 'd)
; d
; > (stack1 'top)
; d
; > (stack1 'push! 'b)
; b
; > (stack1 'top)
; b
; > (stack1 'pop!)
; b
; > (stack1 'pop!)
; d
; > (stack1 'empty?)
; #t