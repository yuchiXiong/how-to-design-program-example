#lang racket

;; 习题 10.2.1 改写函数 contains-doll?，使其输入为库存清单，而不是符号表：
;;    contains-doll? : inventory -> boolean
;;    测定 an-inv 是否包含一条'doll 纪录
;;    (define (contains-doll? an-inv) ...)
;; 同时，改写函数 contains?，使其输入为一个符号和一个库存清单，测定库存清单中是否存在这个符号：
;;    contains? : symbol inventory -> boolean
;;    测定库存清单中是否包含一条 asymbol 纪录
;;    (define (contains? asymbol an-inv) ...)

;; * 定义商品结构体
(define-struct inventory
               (name price picture))

;; * 货物清单
(define inventorys
  (cons
   (make-inventory 'a 4500 'pic1)
   (cons
    (make-inventory 'b 4400 'pic2)
    (cons
     (make-inventory 'doll 4000 'pic3)
     empty))))

;; * 通用map函数
(define (map list callback)
  (cond
    [(empty? list) empty]
    [else
     (cons (callback (first list))
           (map (rest list)
                callback))]))

;; * 获取货物名称表
(map inventorys
     (lambda (x) (inventory-name x)))

;; * 通用includes函数，用来判断target是否存在于给定的list表中
(define (includes list target)
  (cond
    [(empty? list) #f]
    [(symbol=? (first list) target) #t]
    [else
     (includes (rest list) target)]))

;; * 判断商品清单中是否包含名为doll的商品
(define (contains-doll? inventorys)
  (includes
   (map inventorys
        (lambda (x) (inventory-name x)))
   'doll))

(contains-doll? inventorys)
(contains-doll? empty)

;; * 判断商品清单中是否包含名为target的商品
(define (contains? inventorys target)
  (includes
   (map inventorys
        (lambda (x) (inventory-name x)))
   target))

(contains? inventorys 'doll)
(contains? inventorys 'dolll)

;; 习题 10.2.2 给出包含每个物品照片的库存清单的数据定义和结构体定义。说明如何表示图 10.3 所示的库存清单。
;; 设计函数 show-picture，该函数读入一个符号（玩具的名字）和一个上述定义的库存清单，函数返回
;; 相应的玩具照片，如果库存清单中没有此种玩具，返回 false。

;; 通用的find函数
(define (find list callback)
  (cond
    [(empty? list) empty]
    [(callback (first list))
     (first list)]
    [else (find (rest list) callback)]))

(define (show-picture inventorys name)
  (inventory-picture
   (find inventorys
         (lambda (x)
           (symbol=? (inventory-name x)
                     name)))))

(show-picture inventorys 'a)
(show-picture inventorys 'b)
(show-picture inventorys 'doll)

;; 习题 10.2.3 设计函数 price-of，该函数读入一个玩具的名字和一个库存清单，返回该玩具的价格。
(define (price-of inventorys name)
  (inventory-price
   (find inventorys
         (lambda (x)
           (symbol=? (inventory-name x)
                     name)))))
(price-of inventorys 'a)
(price-of inventorys 'b)
(price-of inventorys 'doll)

;; 习题 10.2.4 通讯录建立了人名和电话号码之间的对应关系。给出电话记录和通讯录的数据定义，然后使用这些数据定义设计函数
;;  1． whose-number，给定通讯录以及一个电话号码，查出对应的人名，
;;  2． phone-number，给定通讯录以及一个人名，查出对应的电话号码。
(define-struct phone-book (name phone))
(define phone-books
  (cons
   (make-phone-book 'a 't1234567)
   (cons
    (make-phone-book 'b 't2345678)
    (cons
     (make-phone-book 'c 't3456789)
     (cons
      (make-phone-book 'd 't4567890)
      empty)))))

(define (whose-number phone-books phone)
  (phone-book-name
   (find phone-books
         (lambda (x)
           (symbol=?
            (phone-book-phone x)
            phone)))))
(whose-number phone-books 't1234567)
(whose-number phone-books 't2345678)
(whose-number phone-books 't3456789)
(whose-number phone-books 't4567890)

(define (phone-number phone-books name)
  (phone-book-phone
   (find phone-books
         (lambda (x)
           (symbol=? (phone-book-name x)
                     name)))))
(phone-number phone-books 'a)
(phone-number phone-books 'b)
(phone-number phone-books 'c)
(phone-number phone-books 'd)

;; 习题 10.2.5 定义函数 extract>1，该函数读入一个库存清单，用其中所有售价超过一元的物品建立一个库存清单。

;; 通用filter函数
(define (filter list callback)
  (cond
    [(empty? list) empty]
    [(callback (first list))
     (cons (first list)
           (filter (rest list)
                   callback))]
    [else
     (filter (rest list) callback)]))

(define (extract inventorys)
  (filter inventorys
          (lambda (x)
            (>= (inventory-price x)
                4200))))

(map (extract inventorys)
     (lambda (x) (inventory-name x)))

;; 习题 10.2.7 设计函数 raise-prices，该函数读入一个库存清单，返回一个库存清单，其中所有的商品都涨价5%。
(map inventorys
     (lambda (x)
       (* 1.05 (inventory-price x))))

;; 习题 10.2.8 使用新的库存清单定义，修改习题 10.1.7 中的 recall 函数。该函数读入玩具的名字 ty 和一个库存清单，返回一个库存清单，该清单包含输入中除了名为 ty 外的所有元素。
(define (recall inventorys name)
  (filter inventorys
          (lambda (x)
            (not (symbol=?
                  name
                  (inventory-name
                   x))))))
(map (recall inventorys 'doll)
     (lambda (x) (inventory-name x)))
(map (recall inventorys 'a)
     (lambda (x) (inventory-name x)))
(map (recall inventorys 'b)
     (lambda (x) (inventory-name x)))

;; 习题 10.2.9 使用新的库存清单的定义，修改习题 10.1.6 中的 name-robot 函数。该函数读入一个库存清单，返回一个新的库存清单，其中所有的'robot 被替换成'r2d3。
(define (name-robot inventory
                    current
                    target)
  (map inventory
       (lambda (x)
         (cond
           [(symbol=? (inventory-name x)
                      current)
            (make-inventory
             target
             (inventory-price x)
             (inventory-picture x))]
           [else x]))))

(map (name-robot inventorys 'a 'robot)
     (lambda (x) (inventory-name x)))
(map (name-robot inventorys 'b 'robot)
     (lambda (x) (inventory-name x)))
(map (name-robot inventorys 'c 'robot)
     (lambda (x) (inventory-name x)))
