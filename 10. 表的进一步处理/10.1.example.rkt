#lang racket

(define (map list callback)
  (cond
    [(empty? list) list]
    [else
     (cons (callback (first list))
           (map (rest list)
                callback))]))

(define (filter list callback)
  (cond
    [(empty? list) list]
    [(if (callback (first list))
         (cons (first list)
               (filter (rest list)
                       callback))
         (filter (rest list)
                 callback))]))

;; 习题 10.1.1 如果将每个人的工资提升为每小时 14 元，请问应如何修改图 10.1 中的函数？
(define (salary hours)
  (* hours 14))

(map (cons 1 (cons 2 (cons 3 empty)))
     salary)

;; 习题 10.1.2 没有人能够每周工作 100 小时以上。为了防止欺骗，hours->wages 函数应当对输入进行检查，确保没有一个元素的值超过 100。如果表中某一元素超过了 100，函数应当立即给出错误信息“too many hours”。请问应该如何修改图 10.1 中的函数，使得它能够执行上述真实性检查？
(define (salary2 hours)
  (cond
    [(>= hours 100)
     (error "too many hours")]
    [else (salary hours)]))
(map
 (cons
  3
  (cons
   1
   (cons 10 (cons 10 (cons 10 empty)))))
 salary2)

;; 习题 10.1.3 开发函数 convertFC，该函数把含华氏温度值的表转换成含摄氏温度值的表。
(map
 (cons 232 (cons 683 (cons 473 empty)))
 (lambda (x) (/ (- x 32) 1.8)))

;; 习题 10.1.4 设计函数 convert-euro，基于 1.22 欧元 1 美元的汇率，把美元数值的输入转换成欧元数值。把convert-euro 一般化为 convert-euro-1，该函数读入汇率以及一个美元数额表，使用该汇率把美元数额的表转换成欧元数额的列表。
(map (cons 1 (cons 2 (cons 3 empty)))
     (lambda (x) (* x 1.22)))

;; 习题 10.1.5 设计函数 eliminate-exp，该函数读入数 ua 和玩具价格表 lotp，结果为 lotp 中所有小于等于 ua的元素组成的表。例如，(eliminate-exp 1.0 (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty)))))
;; 预期值：(cons .95 (cons 1.0 empty))
(filter
 (cons
  1
  (cons
   2.95
   (cons 0.95
         (cons 1.0 (cons 5 empty)))))
 (lambda (x) (<= x 1.0)))

;; 习题 10.1.6 设计函数 name-robot，该函数读入一个由玩具名称组成的表，返回一个更精确的玩具名称表，详细说来，就是把表中所有的'robot 替换为'r2d2，其他玩具名称保持不变。
;; 把 name-robot 一般化为函数 substitute。这个新的函数读入两个符号，分别名为 new 和 old 以及一个符号表，返回一个新的符号表，其中所有的 old 都被替换成 new。
;; 例如，(substitute 'Barbie 'doll (cons 'robot (cons 'doll (cons 'dress empty))))
;; 预期值：(cons 'robot (cons 'Barbie (cons 'dress empty)))
(define (substitute new old list)
  (map list
       (lambda (x)
         (if (symbol=? x old) new x))))

(substitute
 'Barbie
 'doll
 (cons
  'robot
  (cons 'doll
        (cons 'dress
              (cons 'aaaa
                    (cons 'bbbb
                          empty))))))

;; 习题 10.1.7 设计函数 recall，从表中去除某些特定的玩具。该函数读入玩具的名字 ty 和表 lon，返回一个表，该表保留了除 ty 以外所有 lon 的元素。
;; 例如：(recall 'robot (cons 'robot (cons 'doll (cons 'dress empty))))
;; 期望值：(cons 'doll (cons 'dress empty))
(filter
 (cons
  'aaa
  (cons
   'bbb
   (cons
    'aaa
    (cons 'ccc (cons 'ddd empty)))))
 (lambda (x) (not (symbol=? x 'aaa))))

;; 习题 10.1.8 设计求解二次方程的函数 quadratic-roots（参见习题 4.4.4 和习题 5.1.4），该函数的输入为方程的系数，即 a、b 以及 c，所执行的计算根据输入而定：
;;  1． 如果 a ＝ 0，输出 'degenerate；
;;  2． 如果 b2 < 4 · a · c，二次方程没有解,在这种情况下，quadratic-roots 返回'none；
;;  3． 如果 b2 ＝ 4 · a · c，二次方程有一个解，这个解就是函数的答案；
;;  4． 如果 b2＞4 · a · c，方程有两个解，函数的返回值是两个数组成的表：第一个解后跟着第二个解。
(define (delta a b c)
  (- (* b b) (* 4 a c)))

(define (quadratic-roots a b c)
  (cond
    [(= a 0) 'degenerate]
    [(< (delta a b c) 0) 'none]
    [(= (delta a b c) 0)
     (- 0 (/ b (* 2 a)))]
    [else
     (cons
      (/
       (- 0 (+ b (sqrt (delta a b c))))
       (* 2 a))
      (cons
       (/ (- (sqrt (delta a b c)) b)
          (* 2 a))
       empty))]))

;; x + 2 = 0
(quadratic-roots 0 1 2)
;; x² + 2 = 0
(quadratic-roots 1 0 2)
;; x² - 4 = 0
(quadratic-roots 1 0 -4)
;; x² = 0
(quadratic-roots 1 0 0)

;; 习题 10.1.9. 在许多杂货店，收银员需要向顾客报出价格。收银员的计算机对于顾客必须支付的金额，构造含有如下五个元素的表：
;;  1． 元的数额；
;;  2． 如果元的数额是 1，这一元素是符号'dollar，否则就是'dollars；
;;  3． 符号'and；
;;  4． 分的数额；
;;  5． 如果分的数额是 1，这一元素是符号'cent，否则就是'cents。
;; 开发函数 controller，该函数读入一个数，返回如上描述的表。例如，如果金额数是＄1.03，那么 (controller 103)的计算过程如下：
;;  (controller 103)
;; 预期值：
;;  (cons 1 (cons 'dollar (cons 'and (cons 3 (cons 'cents empty)))))
;; 提示： Scheme 提供了算术操作 quotient 和 remainder，对于整数 n 和 m，分别生成 n/m 的商和余数。
(define (controller n)
  (cond
    [(= n 0) empty]
    [(>= n 100)
     (cons
      (quotient n 100)
      (cons
       (if (= (remainder n 100) 0)
           'dollar
           'dollars)
       (if (= (remainder n 100) 0)
           (controller
            (remainder n 100))
           (cons
            'and
            (controller
             (remainder n 100))))))]
    [(= n 1)
     (cons 1 (cons 'cent empty))]
    [else
     (cons n (cons 'cents empty))]))

(controller 103)
(controller 1253)
(controller 3)
(controller 1201)
(controller 1)
(controller 100)
