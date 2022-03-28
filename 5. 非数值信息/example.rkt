#lang racket
"hello world"
;; 习题 5.1.2 设计函数 check-guess，输入为两个数，guess 和 target，根据它们的关系，函数产生输出'TooSmall、'Perfect 或 'TooLarge 三者之一。事实上，该函数所实现的是猜数字游戏，在该游戏中，一方在 0 和 99999间随机挑选一个数，而另一方确定该数是那一个数。对于每次猜测，前者的回答就是 check-guess 函数所给出的三种结果之一。
(define RESULT 666)
(define (check_guess number)
  (cond
    [(> number RESULT) 'TooLarge]
    [(= number RESULT) 'Perfect]
    [(< number RESULT) 'TooSmall]))

(check_guess 33)
(check_guess 333)
(check_guess 666)
(check_guess 667)

;; 习题 5.1.4 设计函数 what-kind， 它的参数为一个二次方程的系数，该函数先确定方程是否退化，如果不是，再确定方程有多少个根，因此函数产生下列 4 个符号之一：'degenerate、 'two、'one 或 'none。
(define (delta a b c)
  (- (* b b) (* 4 a c)))
(define (what_kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (delta a b c) 0) 'two]
    [(= (delta a b c) 0) 'one]
    [(< (delta a b c) 0) 'none]))
(what_kind 0 1 2)
(what_kind 1 1 2)
(what_kind 1 1 -2)
(what_kind 1 0 0)

;; 设计函数 check-color，它是猜色游戏的主要部分，游戏参与者之一给两个方块挑选了两个颜色，
;; 它们是游戏的两个目标，游戏的另一个参与者猜测每个方块的颜色，第一个参与者对猜测给出下面四种可能的回答：
;;  1． 'Perfect，如果第一个目标与第一个猜测相符合，第二个目标与第二个猜测相符合；
;;  2． 'OneColorAtCorrectPosition，如果第一个目标与第一个猜测相符合或第二个目标与第二个猜测相符合；
;;  3． 'OneColorOccurs, 猜测的颜色在某一方块出现；
;;  4． 'NothingCorrect， 其他。
;; 第二个参与者的目标是用尽可能少的次数猜测方块的颜色。
;; 函数 check-color 模仿第一个参与者的行为，它的参数是 4 种颜色，为简单起见，假定颜色的类型是符号，如'red，前两个参数是目标，后两个参数是猜测，函数的结果是上述 4 个答案之一。
(define (check_color a1 a2 b1 b2)
  (cond
    [(and (symbol=? a1 b1) (symbol=? a2 b2))
     'Perfect]
    [(or (symbol=? a1 b1)
         (symbol=? a2 b2))
     'OneColorAtCorrectPosition]
    [(or (symbol=? a1 b1)
         (symbol=? a1 b2)
         (symbol=? a2 b1)
         (symbol=? a2 b2))
     'OneColorOccurs]
    [else 'NothingCorrect]))

(check_color 'red 'red 'blue 'blue)
(check_color 'red 'red 'red 'blue)
(check_color 'red 'red 'red 'red)
(check_color 'blue 'blue 'red 'red)
(check_color 'blue 'red 'yellow 'green)
(check_color 'blue 'red 'red 'green)
