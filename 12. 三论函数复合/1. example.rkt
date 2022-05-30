#lang racket

(define (sort list func)
  (cond
    [(empty? list) empty]
    [(cons? list)
     (insert (first list)
             (sort (rest list) func)
             func)]))

(define (insert n list func)
  (cond
    [(empty? list) (cons n empty)]
    [(cons? list)
     (cond
       [(func n (first list))
        (cons n list)]
       [else
        (cons (first list)
              (insert n
                      (rest list)
                      func))])]))

(define list
  (cons
   9
   (cons
    8
    (cons
     10
     (cons 1
           (cons 2 (cons 0 empty)))))))

(sort list (lambda (a b) (>= a b)))

;;; 习题 12.2.1 设计一个程序，按照日期对邮件进行排序。邮件结构体的定义如下：
(define-struct mail (from date message))
;;; mail-message 是结构体：(make-mail name n s) 其中 name 是字符串，n 是数，s 也是字符串。
;;; 另外再开发一个程序，按照名称对邮件进行排序。请使用 string<?比较两个字符串的大小。
(define mails
  (cons
   (make-mail "yuchi1" 100 "hello1")
   (cons
    (make-mail "yuchi2" 80 "hello2")
    (cons
     (make-mail "yuchi3" 10 "hello3")
     (cons
      (make-mail "yuchi4" 1100 "hello4")
      empty)))))

(sort mails
      (lambda (a b)
        (>= (mail-date a)
            (mail-date b))))

(sort mails
      (lambda (a b)
        (string<? (mail-from a)
                  (mail-from b))))

;; 习题 12.2.2 函数 search：
;; search : number list-of-numbers -> boolean
(define (search n alon)
  (cond
    [(empty? alon) false]
    [else
     (or (= (first alon) n)
         (search n (rest alon)))]))
;; 判断一个数是否在一个表中出现。如果表不含该数，函数需要遍历整个表才能得出结论。利用表的有序性，开发函数 search-sorted，判断某个数是否在一个有序表中出现。
;; 术语: 在程序设计书籍中，称函数 search-sorted 实施的查找为线性查找。
(define (search-sorted n alon)
  (cond
    [(empty? alon) false]
    [(> n (first alon)) false]
    [else
     (or
      (= (first alon) n)
      (search-sorted n (rest alon)))]))

(search-sorted
 3
 (cons
  1
  (cons 3 (cons 4 (cons 10 empty)))))

;; 习题 12.3.1 设计辅助函数 add-at-end，其将表的第一个元素加到表的末端，接着修改函数 draw-polygon。
(define (add-at-end n list)
  (cond
    [(empty? list) (cons n empty)]
    [else
     (cons
      (first list)
      (add-at-end n (rest list)))]))

(define list1
  (cons 1 (cons 2 (cons 3 empty))))

(add-at-end (first list1) list1)
