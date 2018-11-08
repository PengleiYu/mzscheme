(require (lib "defmacro.ss"))

;;;查找列表l中对象o的位置
(define list-position
  (lambda (o l)
	(let loop ((i 0)   (l l))
	  (if (null? l) #f
		(if (eqv? (car l) o) i
		  (loop (+ i 1)   (cdr l)))))))


;;;定义结构宏
;;;和教程不同，mzscheme中if必须有else分支，所以只好为if的空分支添加了void
;;;这导致定义结构时可能产生大量换行
(define-macro defstruct
			  (lambda (s . ff);s是类名，ff是属性名列表，可能包含属性默认值
				(let ((s-s (symbol->string s)) (n (length ff)))
				  (let* ((n+1 (+ n 1))
						 (vv (make-vector n+1)));创建n+1长度的向量vv用于存储默认值
					;为向量存入定义结构时的默认值，若无则为空
					(let loop ((i 1) (ff ff))
					  (if (<= i n)
						(let ((f (car ff)));遍历列表每一项
						  (vector-set! vv i;为向量中对应位置赋值
									   ;若该项为pair，则取pair的cadr，否则为空
									   (if (pair? f) (cadr f) '(if #f #f (void))))
						  (loop (+ i 1) (cdr ff)))
						(void)))
					;将ff映射为每一项的第一个元素，即定义结构时的变量名
					(let ((ff (map (lambda (f) (if (pair? f) (car f) f));属性名列表ff
								   ff)))
					  `(begin
						 (define ,(string->symbol
									(string-append "make-" s-s));开始定义make函数
						   (lambda fvfv;make函数的形参fvfv
							 (let ((st (make-vector ,n+1)) (ff ',ff));开始定义表示对象的向量st
							   (vector-set! st 0 ',s);开头保存类名
							   ,@(let loop ((i 1) (r '()));st按顺序存入默认值
								   (if (>= i n+1) r
									 (loop (+ i 1)
										   (cons `(vector-set! st ,i;这个cons没看懂，r是怎么被赋值的？
															   ,(vector-ref vv i))
												 r))))
							   (let loop ((fvfv fvfv));遍历形参
								 (if (not (null? fvfv))
								   (begin
									 ;由这句可知make函数接受的是属性名属性值配对的列表
									 ;可以只传入部分属性
									 (vector-set! st
												  ;根据属性名找到位置,然后赋值
												  (+ (list-position (car fvfv) ff)
													 1)
												  (cadr fvfv))
									 (loop (cddr fvfv)))
								   (void)
								   ))
							   st)));返回对象st
						 ,@(let loop ((i 1) (procs '()));生成所有属性的setter和getter
							 (if (>= i n+1) procs
							   (loop (+ i 1)
									 (let ((f (symbol->string
												(list-ref ff (- i 1)))))
									   (cons;由以下代码可知setter和getter其实都是根据位置进行操作的
										 `(define ,(string->symbol
													 (string-append
													   s-s "." f));定义每个属性的getter
											(lambda (x) (vector-ref x ,i)))
										 (cons
										   `(define ,(string->symbol
													   (string-append
														 "set!" s-s "." f));定义每个属性的setter
											  (lambda (x v)
												(vector-set! x ,i v)))
										   procs))))))
						 (define ,(string->symbol (string-append s-s "?"));定义类型判断函数
						   (lambda (x)
							 (and (vector? x)
								  (eqv? (vector-ref x 0) ',s))))))))))
