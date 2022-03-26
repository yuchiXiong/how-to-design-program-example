#lang racket
;; 习题 2.1.1 查明 DrScheme 是否具备平方、计算一个角度的正弦值以及确定两个数的最大值的运算。
(sqrt 625)
(cos 3.1415926)
(sin 1.5)
(max 100 300)

;; 习题 2.1.2 在 DrScheme 中计算 (sqrt 4)、 (sqrt 2) 和 (sqrt -1)。再查明 DrScheme 是否包含计算一个角度的正切值的运算。
(sqrt 4)
(sqrt 2)
(sqrt -1)
(tan 1)

;; 习题 2.2.1 定义程序 Fahrenheit -> Celsius，输入为华氏温度值，输出为等值的摄氏温度值。请查看化学或物理书籍了解温度的转换公式。
;; number -> number
(define (fahrenheit_to_celsius
         fahrenheit)
  (/ (- fahrenheit 32) 1.8))
(fahrenheit_to_celsius 1)

;; 习题2.2.2 定义程序 dollar-> euro，程序输入为美元额，输出为等价的欧元额。请查阅报纸了解美元关于欧元的汇率。
;; number -> number
(define (dollar_to_euro dollar)
  (* dollar 0.9107))
(dollar_to_euro 100)

;; 习题2.2.3 定义程序 triangle, 程序输入为一个三角形的底和高的长度，输出为三角形的面积，请查阅平面几何书籍了解三角形面积的计算公式。
;; number number -> number
(define (triangle a b)
  (/ (* a b) 2))
(triangle 3 4)

;; 海伦公式 通过三角形的三边求三角形的面积
;; number number number -> number
(define (heron_p a b c)
  (/ (+ a b c) 2))
(define (heron_area a b c)
  (sqrt (* (heron_p a b c)
           (- (heron_p a b c) a)
           (- (heron_p a b c) b)
           (- (heron_p a b c) c))))
(heron_area 3 4 5)

;; 习题2.2.4 定义程序convert3，输入为3个数，分别是一个数的个位，十位，百位上的数码，程序输出为该数。
;; 例如（Convert3 1 2 3）的输出为321。
;; 请使用代数书籍了解该转换过程。
;; number number number -> 
(define (convert3 a b c)
  (+ (* c 100) (* b 10) a))
(convert3 3 5 2)

;; 习题2.2.5 经典数字小说要求读者分别在 n = 2、 n = 5 和 n = 9 时计算公式
;; n² + 10
(define (calculate1 n)
  (+ (* n n) 10))
;; ( 1 / 2) * n² + 20
(define (calculate2 n)
  (+ (* (/ 1 2) (* n n)) 20))
;; 2 - ( 1 / n )
(define (calculate3 n)
  (- 2 (/ 1 n)))

;; 习题2.3.1 在乌托邦计算所得税的税率是固定的，为毛收入的15%。试编写程序 tax，按照雇员的毛收入计算所得税。继续编写程序 netpay, 计算雇员的税后所得。假定雇员的每小时工资为 12 美元。
;; number -> number
(define (tax gross)
  (* 0.15 gross))
(define (netpay hour)
  (- (* hour 12) (tax (* hour 12))))

;; 习题2.3.2 假定当地超级市场需要-一个程序计算一袋硬币的价值。编写程序 sum_coins，其输入为钱袋中1美分、5美分、10美分和25美分的硬币数，输出为钱袋中硬币的价值总额。
;; number number number number -> number
(define (sum_coins a b c d)
  (+ (* a 1) (* b 5) (* c 10) (* d 25)))

;; 习题2.3.3 某旧式电影院有-一个简单的收益计算:每张电影票价格为5美元，每趟电影放映开销20美元，每位观众耗费0.5美元。试编写程序 total_profit， 输入为一场电影的观众数，输出为电影院的净收入。
;; number -> number
(define (total_profit audience_count)
  (- (* audience_count 5)
     (* audience_count 0.5)
     20))

;; 习题2.4.1/2.4.2/2.4.3
(+ 10 20)
(define (f x)
  (+ x 10))
(define (g x)
  (+ x 10))
;;; (+ 5 (/ 1 0))

