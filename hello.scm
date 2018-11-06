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
