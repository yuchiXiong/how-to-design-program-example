#lang racket
;; 习题 9.1.1 创建表示以下对象的表
;;  太阳系中的所有行星；
;;  早餐菜谱：牛排、蚕豆、面包、水、果汁、白乳酪和冰淇淋；
;;  基本颜色。

(define list
  (cons 1 (cons 2 (cons 3 4))))

(cons
 'sun
 (cons 'mercury
       (cons 'breakfast
             (cons 'colors
                   (cons 'nil 'nil)))))

(cons
 'breakfast
 (cons
  'beans
  (cons
   'bread
   (cons 'water
         (cons 'juice
               (cons 'milk
                     'ice-cream))))))

(define list1
  (cons
   1
   (cons 2 (cons 3 (cons 4 empty)))))

(rest list1)
(first (rest list1))
(rest (rest list1))
(first (rest (rest list1)))
(rest (rest (rest list1)))

;; 习题 9.1.3 完成 add-up-3 的设计：先定义主体，然后用一些例子进行测试。
;; 由 3 个数组成的表可以表示三维空间中的一个点。计算三维空间中某个点到原点的距离的方法与二维空间没什么两样，即把所有坐标自乘，相加，再求和的平方根。使用 add-up-3 的模板设计函数 distance-to-0-for-3，该函数计算三维空间中某个点到原点的距离。
(define (add-up-3 a b c)
  (sqrt (+ (* a a) (* b b) (* c c))))
(add-up-3 2 2 2)

;; 习题 9.1.4 给出由两个符号组成的表的数据定义，再设计函数 contains-2-doll?，该函数读入包含两个符号的表，判断两个符号中是不是有一个为'doll。
(define (contains-2-doll sym_list)
  (cond
    [(empty? sym_list) #f]
    [(cons? (first sym_list))
     (contains-2-doll (first sym_list))]
    [(cons? (rest sym_list))
     (contains-2-doll (rest sym_list))]
    [(not (cons? (first sym_list)))
     (symbol=? 'doll (first sym_list))]
    [else #f]))

(define sym_list
  (cons 'doll
        (cons 'a (cons 'b empty))))

(define sys_list_without_doll
  (cons 'a (cons 'b (cons 'c empty))))

(contains-2-doll sym_list)
(contains-2-doll sys_list_without_doll)

;; 习题 9.3.1 在 DrScheme 中使用下面列表测试 contains-doll? 的定义：
(contains-2-doll empty)
(contains-2-doll (cons 'ball empty))
(contains-2-doll
 (cons 'arrow (cons 'doll empty)))
(contains-2-doll
 (cons 'bow
       (cons 'arrow
             (cons 'ball empty))))

;; cons -> number
(define (sum list)
  (cond
    [(empty? list) 0]
    [else
     (+ (first list)
        (sum (rest list)))]))

;; 习题 9.5.1 使用下列表测试 sum：
;;  empty
;;  (cons 1.00 empty)
;;  (cons 17.05 (cons 1.22 (cons 2.59 empty)))
;;  (cons 2.59 empty)
;;  (cons 1.22 (cons 2.59 empty))

(sum empty)
(sum (cons 1.00 empty))
(sum (cons
      17.05
      (cons 1.22 (cons 2.59 empty))))
(sum (cons 2.59 empty))
(sum (cons 1.22 (cons 2.59 empty)))

;; 习题 9.5.2 设计函数 how-many-symbols，该函数读入一个符号表，返回表中元素的数目。设计函数
;; how-many-numbers，该函数读入一个数表，返回表中元素的数目。思考一下 how-many-symbols 和
;; how-many-numbers 有什么差别？
;; how-many-symbols cons -> number
(define (how-many-symbols sym_list)
  (cond
    [(empty? sym_list) 0]
    [(symbol? (first sym_list))
     (+ 1
        (how-many-symbols
         (rest sym_list)))]
    [else
     (how-many-symbols
      (rest sym_list))]))

(how-many-symbols
 (cons 'a (cons 'b empty)))
(how-many-symbols
 (cons
  'a
  (cons 'b (cons 'c (cons 'd empty)))))

;; how-many-numbers cons -> number
(define (how-many-number sym_list)
  (cond
    [(empty? sym_list) 0]
    [(number? (first sym_list))
     (+ 1
        (how-many-number
         (rest sym_list)))]
    [else
     (how-many-number
      (rest sym_list))]))

(how-many-number
 (cons
  'a
  (cons 'b (cons 'c (cons 'd empty)))))
(how-many-number
 (cons
  'a
  (cons 'b (cons 'c (cons 1 empty)))))

;; 习题 9.5.3 设计函数 dollar-store?，该函数读入一个物价表，检查是否所有的价格都小于 1。
;;  例如，下列表达式的计算结果应当是 true：
;;  (dollar-store? empty)
;;  (not (dollar-store? (cons .75 (cons 1.95 (cons .25 empty)))))
;;  (dollar-store? (cons .75 (cons .95 (cons .25 empty))))
;; 进一步设计一个更一般的函数，该函数读入一个物价表和限价，检查物价表中所有价格是不是都小于限价。
(define (dollar-store? list limit)
  (cond
    [(empty? list) #t]
    [(< (first list) limit)
     (dollar-store? (rest list) limit)]
    [else #f]))

(dollar-store? empty 1.00)
(dollar-store?
 (cons .75
       (cons 1.95 (cons 0.25 empty)))
 1.00)
(dollar-store?
 (cons .75 (cons .95 (cons .25 empty)))
 1.00)

;; 习题 9.5.4 设计函数 check-range1，该函数读入由温度测量值组成的表，检查是否所有的温度值都在 5oC 和95oC 之间。把这个函数一般化为 check-range，该函数读入由温度测量值组成的表和一个区间，检查是否所有的温度测量值都落在该区间。
(define (check-range list
                     limit-min
                     limit-max)
  (cond
    [(empty? list) #t]
    [(and (< (first list) limit-max)
          (>= (first list) limit-min))
     (check-range (rest list)
                  limit-min
                  limit-max)]
    [else #f]))

(check-range empty 5.00 95.00)
(check-range
 (cons 4 (cons 5 (cons 6 empty)))
 5.00
 95.00)
(check-range
 (cons 5 (cons 5 (cons 6 empty)))
 5.00
 95.00)
(check-range
 (cons 5 (cons 95 (cons 6 empty)))
 5.00
 95.00)

;; 习题 9.5.5 设计函数 convert，该函数读入一个数表，返回对应的数。表中的第一个数是数的最低位，以此类推。
;; 开发函数 check-guess-for-list，实现习题 5.1.3 中猜数字游戏的一般版本。该函数读入两个输入：数表guess，代表玩家的猜测；数 target，代表随机生成的隐含数。根据数（用 convert）转换成的数与 target 的关系，输出三种结果之一：'TooSmall，'Perfect 或者是'TooLarge。
(define (convert list)
  (cond
    [(empty? list) 0]
    [(number? (first list))
     (+ (first list)
        (* 10 (convert (rest list))))]
    [else
     (error
      "list is not a number list")]))

(convert
 (cons 1 (cons 2 (cons 3 empty))))
(convert
 (cons
  1
  (cons 2 (cons 3 (cons 4 empty)))))

(define (check-guess-for-list list
                              target)
  (cond
    [(convert list)
     (cond
       [(= (convert list) target)
        'Prefect]
       [(> (convert list) target)
        'TooSmall]
       [(< (convert list) target)
        'TooLarge])]
    [else
     (error
      "list is not a number list")]))

(check-guess-for-list
 (cons 1 (cons 2 (cons 3 empty)))
 321)

;; 习题 9.5.6 设计函数 delta，该函数读入两个价格表（也就是数表）。第一个表代表月初的库存清单，第二个表代表月末的库存清单。函数的输出是两个价格的差，如果价格上涨了，其值就是正的；如果价格下跌了，其值就是负的。

;; 习题 9.5.7 定义函数 average-price，它的输入是一个价格表，函数计算玩具的平均价格。平均价格是总的价格除以玩具的数量。
(define (average-price list)
  (/ (sum list) (how-many-number list)))

(average-price
 (cons 1 (cons 2 (cons 3 empty))))
(average-price
 (cons
  9
  (cons
   9
   (cons 3 (cons 8 (cons 3 empty))))))
