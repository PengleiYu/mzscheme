(require (lib "defmacro.ss"))

(define list-position
  (lambda (o l)
	(let loop ((i 0)   (l l))
	  (if (null? l) #f
		(if (eqv? (car l) o) i
		  (loop (+ i 1)   (cdr l)))))))


;;;定义结构宏
;;;和教程不同，mzscheme中if必须有else分支，所以只好为if的空分支添加了newline
;;;这导致定义结构时可能产生大量换行
(define-macro defstruct
			  (lambda (s . ff)
				(let ((s-s (symbol->string s)) (n (length ff)))
				  (let* ((n+1 (+ n 1))
						 (vv (make-vector n+1)))
					(let loop ((i 1) (ff ff))
					  (if (<= i n)
						(let ((f (car ff)))
						  (vector-set! vv i
									   (if (pair? f) (cadr f) '(if #f #f (newline))))
						  (loop (+ i 1) (cdr ff)))
						(newline)
						))
					(let ((ff (map (lambda (f) (if (pair? f) (car f) f))
								   ff)))
					  `(begin
						 (define ,(string->symbol
									(string-append "make-" s-s))
						   (lambda fvfv
							 (let ((st (make-vector ,n+1)) (ff ',ff))
							   (vector-set! st 0 ',s)
							   ,@(let loop ((i 1) (r '()))
								   (if (>= i n+1) r
									 (loop (+ i 1)
										   (cons `(vector-set! st ,i
															   ,(vector-ref vv i))
												 r))
									 ))
							   (let loop ((fvfv fvfv))
								 (if (not (null? fvfv))
								   (begin
									 (vector-set! st
												  (+ (list-position (car fvfv) ff)
													 1)
												  (cadr fvfv))
									 (loop (cddr fvfv)))
								   (newline)
								   ))
							   st)))
						 ,@(let loop ((i 1) (procs '()))
							 (if (>= i n+1) procs
							   (loop (+ i 1)
									 (let ((f (symbol->string
												(list-ref ff (- i 1)))))
									   (cons
										 `(define ,(string->symbol
													 (string-append
													   s-s "." f))
											(lambda (x) (vector-ref x ,i)))
										 (cons
										   `(define ,(string->symbol
													   (string-append
														 "set!" s-s "." f))
											  (lambda (x v)
												(vector-set! x ,i v)))
										   procs))))))
						 (define ,(string->symbol (string-append s-s "?"))
						   (lambda (x)
							 (and (vector? x)
								  (eqv? (vector-ref x 0) ',s))))))))))
