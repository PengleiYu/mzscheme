(load-relative "myLib.scm")

;第九章：结构

;定义一个结构
(defstruct tree height girth age leaf-shape leaf-color)

;创建结构同时赋值部分属性
(define coconut
  (make-tree 'height 30
			 'leaf-shape 'frond
			 'age 5))

;打印属性
(tree.height coconut)
(tree.leaf-shape coconut)
(tree.girth coconut)

;修改属性
(set!tree.height coconut 40)
(set!tree.girth coconut 10)

(tree.height coconut)
(tree.girth coconut)

;定义结构时给予属性默认值
(defstruct tree height girth age
  (leaf-shape 'frond)
  (leaf-color 'green))

;创建结构
(define palm (make-tree 'height 60))

(tree.height palm)
(tree.leaf-shape palm)
(tree.leaf-color palm)

;创建结构，覆盖默认值
(define plantain
  (make-tree 'height 7
			 'leaf-shape 'sheet))

(tree.height plantain)
(tree.leaf-shape plantain)
(tree.leaf-color plantain)

;第十章：关联表和表格

(define foo (list '(a . 1) '(b . 2) '(c . 3)))

(assv 'b foo)

;定义表结构
(defstruct table (equ eqv?) (alist '()))
;;;从table中查找k的值，默认为d的car，比较器在table中已定义
(define table-get
  (lambda (tbl k . d)
	(let ((c (lassoc k (table.alist tbl)  (table.equ tbl))));定义c为tbl中key为k的pair
	  (cond (c (cdr c));若c存在，则返回cdr即value
			((pair? d)  (car d))))));c不存在，且默认值d为pair则返回d的car
;;;从列表al中查找k对应的value，比较器为参数equ?
(define lassoc
  (lambda (k al equ?);从pairList的al中查找key为k的pair，没有则返回#f
	(let loop ((al al))
	  (if (null? al) #f
		(let ((c (car al)))
		  (if (equ? (car c) k) c
			(loop (cdr al))))))))
;;;向table中指定的key赋值value
(define table-put!
  (lambda (tbl k v)
	(let ((al (table.alist tbl)))
	  (let ((c (lassoc k al (table.equ tbl))))
		(if c (set-cdr! c v);若tbl中已存在key为k的pair，则更新其value
		  (set!table.alist tbl (cons (cons k v) al)))))));否则将tbl更新为开头插入新pair的list
;;;该函数接受一个table和一个两参数的过程p，
;;;然后遍历table每一个pair，对pair应用过程p
(define table-for-each
  (lambda (tbl p)
	(for-each
	  (lambda (c)
		(p (car c)  (cdr c)))
	  (table.alist tbl))))

;;; todo 未运行成功
(define foo '((a . 1) (b . 2) (c . 3)))

(for-each myPrint foo)

(table-put! foo 'a 111)

(table.alist foo)

(table.equ foo)

(define myPrint 
  (lambda (x)
	(display (car x))
	(display '-)
	(display (cdr x))
	(newline)))

(table-get foo 'b )

;todo 知道问题在哪了；defstruct中创建的是vector，这里当做list处理了，明天再改
