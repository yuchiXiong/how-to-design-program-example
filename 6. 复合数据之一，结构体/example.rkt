#lang racket
;; 习题 6.3.3 给出表示喷气式飞机的结构体定义，假定飞机的基本属性有四：名称('f22、'tornado 或 'mig22)、加速度、 最高时速和航程。设计函数 within-range，输入为飞机记录和目标离开基地的距离，函数确定飞机是否可以到达指定目标。进一步开发程序 reduce-range，输入为飞机记录，输出也是飞机记录，但其中 range字段的值为原始值的 80%。
(define-struct airplane
               (name acceleration
                     top-speed
                     voyage))
(define plane
  (make-airplane 'f22 100 900 100))
(airplane-voyage plane)
(define (within_range plane distance)
  (> (airplane-voyage plane) distance))

(within_range plane 99)

(define (reduce_range plane)
  (make-airplane
   (airplane-name plane)
   (airplane-acceleration plane)
   (airplane-top-speed plane)
   (* (airplane-voyage plane) 0.8)))

(airplane-voyage plane)
(airplane-voyage (reduce_range plane))

;; 习题 6.4.2 假定一个时刻由 3 个数组成：时、分、秒。请给出结构体定义和数据定义，用来表示从午夜开始计算的时刻。
;; number number number
(define-struct time
               (hour minute second))

;; 练习 6.4.3 假定字是'a 到 'z 之间的字符，请给出由 3 个字母组成的单词的结构体定义和数据定义。
;; char char char
(define-struct word (a b c))

;; 习题 6.5.2 设计函数 time->seconds，它读入一个 time 结构体（参见练习6.4.2），返回从午夜至 time 结构体所表示的时刻之间的秒数，例如：(time->seconds (make-time 12 30 2)) 预期值：45002
(define (time->seconds time)
  (+ (* 3600 (time-hour time))
     (* 60 (time-minute time))
     (time-second time)))
(time->seconds (make-time 12 30 2))
