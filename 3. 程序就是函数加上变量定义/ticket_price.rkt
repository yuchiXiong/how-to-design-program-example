#lang racket
;; 习题3.1.1 对每个函数给出计算实例。例如，确定当票价为3美元、4美元或5美元时有多少人愿意买票看电影。使用实例可以了解从票价计算观众数的一般规则。在需要的时候还可以尝试更多的例子。
;; 习题3.1.2 使用习题3.1.1 的结果计算当票价为3美元、4美元和5美元时，放映电影的开销，进一步计算在上述票价下放映电影的收入，最后计算业主在每种情况下的收益。考虑问题:若要使收益最大，要将票价定为多少?
;; 习题3.1.3分别使用在图3.1两栏中定义的函数计算当业主将票价定为3美元、4美元和5美元时的收益，确认结果和习题3.1.2中所预期的相同。
;; 习题3.1.4研究了放映电影的开销结构后，业主发现了几种降低开销的方法。其中之一是，取消固定开销，对每个观众支付1.5美元给供片商，请修改程序以反映这种改动。修改程序后，用3美元、4美元和5美元票价测试程序并进行比较。

;; 给定票价计算利润 number -> number
(define (profit ticket_price)
  (- (revenue ticket_price)
     (cost ticket_price)))

;; 给定票价计算总收入 number -> number
(define (revenue ticket_price)
  (* ticket_price
     (attendees ticket_price)))

;; 给定票价计算总支出 number -> number
(define (cost ticket_price)
  (+ 180
     (* 0.04 (attendees ticket_price))))

;; 给定票价计算观众数 number -> number
(define (attendees ticket_price)
  (+ (* (/ (- 5 ticket_price) 0.1) 15)
     120))

(profit 3)
(profit 4)
(profit 5)

(define PI 3.14)
(* PI 10)
