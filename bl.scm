#!/usr/bin/scheme --script
(define (puts x) (display x)(newline))
(define table (make-hash-table))
(define (my-apply object message args table)
	(define inside-table (get-hash-table table message 'error))
	(set! object-value (my-eval object table))
	(set! args-value (my-eval args table))
	(set! object-key (get-hash-table inside-table 'object-key 'error))
	(set! args-key (get-hash-table inside-table 'args-key 'error))
	(if (eq? (get-hash-table inside-table 'args-key! 'error) args-value)
		(my-eval (get-hash-table inside-table 'inside-value 'error) table)
		(let []
			(define i-table (make-hash-table))
			(put-hash-table! i-table object-key object-value)
			(put-hash-table! i-table args-key args-value)
			(put-hash-table! i-table message inside-table)
			(my-eval
					`(
					,(my-eval (get-hash-table inside-table 'object-value 'error) i-table)
					,(get-hash-table inside-table 'message-value 'error)
					,(my-eval (get-hash-table inside-table 'args-value 'error) i-table)
				)
				table
			)

		)
	)

)
(define (my-eval exp table)
	(cond 
		((symbol? exp) (get-hash-table table exp 'error))
		((number? exp) exp)
		(else
			(let [
			(object (car exp))
			(message (cadr exp))
			(args #f)]
			(if (eq? message '=>)
				(if (number? (caddr exp))
					(let*[
							(object-key (car object))
							(message-key (cadr object))
							(args-key (caddr object))	
							(args (caddr exp))
							(inside-value args)	
						]
						(if (eq? 'error (get-hash-table table message-key 'error))
							(let [(inside-table (make-hash-table))]
								(put-hash-table! inside-table 'object-key object-key)
								(put-hash-table! inside-table 'message-key message-key)
								(put-hash-table! inside-table 'args-key! args-key)
								(put-hash-table! inside-table 'inside-value inside-value)
								(put-hash-table! table message-key inside-table)
							)
							(let[(inside-table (get-hash-table table message-key 'error))]
								(put-hash-table! inside-table 'object-key object-key)
								(put-hash-table! inside-table 'message-key message-key)
								(put-hash-table! inside-table 'args-key! args-key)
								(put-hash-table! inside-table 'inside-value inside-value)
								(put-hash-table! table message-key inside-table)
							)
						)
					)	
					(let*[
						(object-key (car object))
						(message-key (cadr object))
						(args-key (caddr object))
						(args (caddr exp))
						(object-value (car args))
						(message-value (cadr args))
						(args-value (caddr args))
						
						]
						(if (eq? 'error (get-hash-table table message-key 'error))
							(let [(inside-table (make-hash-table))]
								(put-hash-table! inside-table 'object-key object-key)
								(put-hash-table! inside-table 'message-key message-key)
								(put-hash-table! inside-table 'args-key args-key)
								(put-hash-table! inside-table 'object-value object-value)
								(put-hash-table! inside-table 'message-value message-value)
								(put-hash-table! inside-table 'args-value args-value)
								(put-hash-table! inside-table 'args-exp args)
								(put-hash-table! table message-key inside-table)
							)
							(let[(inside-table (get-hash-table table message-key 'error))]
								(put-hash-table! inside-table 'object-key object-key)
								(put-hash-table! inside-table 'message-key message-key)
								(put-hash-table! inside-table 'args-key args-key)
								(put-hash-table! inside-table 'object-value object-value)
								(put-hash-table! inside-table 'message-value message-value)
								(put-hash-table! inside-table 'args-value args-value)
								(put-hash-table! inside-table 'args-exp args)
								(put-hash-table! table message-key inside-table)
							)
						)

					)
				)
				(let []
					(if (and (symbol? object)
						(not (eq? 'error (get-hash-table table object 'error))))
						(set! object (get-hash-table table object 'error)))
					(if (eq? (length exp) 3) (set! args (caddr exp)))
					(if (list? object) (set! object (my-eval object table)))
					(if (list? args) (set! args (my-eval args table)))
					(if (and (symbol? args)
						(not (eq? 'error (get-hash-table table args 'error))))
						(set! args (get-hash-table table args 'error)))
					(cond
						((eq? message ':=)
							(put-hash-table! table object args))
						((eq? message 'show)
							(puts object)
						)
						((eq? message '=)
							(= object args))
						((eq? message '+)
							(+ object args))
						((eq? message '-)
							(- object args))
						((eq? message '*)
							(* object args))
						((eq? message '/)
							(/ object args))
						(else 
							(my-apply object message args table)))))))))


(define file (cadr (command-line)))
(define stream (open-input-file file))
(define (read-line stream line)
	(define (lop line stream)
		(if (or (eq? (peek-char stream) #\newline) (eq? (peek-char stream) #!eof))
			(let[]
				(if (eq? (peek-char stream) #!eof)
					(let[]
						(set! state #f)
						line)
					(let[] 
						(read-char stream)
						line
					)
				)
			)
			(let()(set! line
				(string-append
					line
					(make-string 1 (read-char stream))
				)
				)
				(lop line stream)
			)
		)
	)
	(lop line stream)
)

(define (token statment)
	(string-append "(" statment ")"))
(define state)
(define (eval-it stream table li)
	(define statment (read-line stream ""))
	(set! li (read (open-input-string (token statment))))
	(if (not (null? li)) (my-eval li table))
	(if (not (eq? state #f))
		(if (string=? statment "")
			(eval-it stream table li)
				(let[]
					
					(eval-it stream table li)
				)
		)
	)
)
;(load "fun.scm")
;(my-eval '((x ss y) => ((x * x) + ((2 * (x * y)) + (y * y)))) table)

;(my-eval '(1 ss 2) table)
; (my-eval '((x ** y) => (x * (x ** (y - 1)))) table)
; (my-eval '((x ** 0) => 1) table)
; (puts (my-eval '(3 ** 3) table))

(eval-it stream table '())
; x的y次方等于x乘以x的y减1次方
; x的0次方等于1

; x ^ y => x ^ (y / 2)



; 3 ** 5
; = 3 * (3 ** 4)
; = 3 * (3 * (3 * 3))
; = 3 * (3 * (3 * (3 ** 2)))
; = 3 * (3 * (3 * (3 * (3 ** 1))))
; = 3 * (3 * (3 * (3 * (3 * (3 ** 0)))))

; 3 ** 2 = 9
