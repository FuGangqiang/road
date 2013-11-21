#|
    data13.scm -- 坐标高程数据
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

;; 可以把坐标、高程数据事先存储在 data.scm 中，当加载 (road xy) 或
;; (road h) 库时 data.scm 将会被读入，data.scm 中的坐标数据是一个由数个
;; vector 组成的一个 list，每一个 vector 就是一个线元（直线、圆曲线或缓
;; 和曲线）坐标正反计算的元数据。

(define *data-h-y*
  ;; 右线高程元数据
  ;; 数据中，若半径为负，则曲线为凸曲线；若半径为正，则曲线为凹曲线。
  ; #(右线里程 高程 曲线半径 切线长 坡度)
  ; #(L H R T I)
  '(#(52300 803.220 +200000 200  +0.019)
    #(58260 916.460 -15000  345  -0.027)))

(define *data-h-z*
  ;; 左线高程数据
  ;; 数据中，若半径为负，则曲线为凸曲线；若半径为正，则曲线为凹曲线。
  ; #(左线里程 高程 曲线半径 切线长 坡度)
  ; #(L H R T I)
  '(#(52300 803.220 +200000 200 +0.019)
    #(58260 916.460 -15000  345 -0.027)))

(define *data-xy-y*
  ;; 右线坐标元数据
  ;; 数据中，若半径为正，曲线向右转；若半径为负，曲线向左转。
  ; #(右线里程 坐标 方位角(d m s) 起点半径 终点半径 缓和曲线长度)
  ; #(l xy deg r1 r2 ls)
  '(#(51848.321 2888192.374+499744.834i (197 52 19.9)  0     0     #f)   ; 直线
    #(52684.641 2887396.412+499488.172i (197 52 19.9)  0     -1000 150)  ; 正向缓和曲线
    #(52834.641 2887252.580+499445.731i (193 34 30)    -1000 -1000 #f)   ; 圆曲线
    #(53386.020 2886708.568+499466.828i (161 58 59.9)  0     -3000 170)  ; 正向缓和曲线
    #(53556.020 2886550.817+499529.939i (155 29 23.2)  -3000 -3000 #f)   ; 圆曲线
    #(54327.625 2885897.398+499936.317i (140 45 11.5)  0      0    #f)   ; 直线
    ))

(define *data-xy-z*
  ;; 左线坐标元数据
  ;; 数据中，若半径为正，曲线向右转；若半径为负，曲线向左转。
  ; #(右线里程 坐标 方位角(d m s) 起点半径 终点半径 缓和曲线长度)
  ; #(l xy deg r1 r2 ls)
  '(#(52022.713 2888018.361+499739.583i (200 21 42.5)  0     0     #f)
    #(52649.728 2887430.526+499521.415i (200 21 42.5)  0     -983  150)
    #(52799.728 2887288.654+499472.829i (195 59 25.1)  -983  -983  #f)
    #(53325.607 2886769.058+499466.803i (165 20 18.9)  0     -1800 150)
    #(53475.607 2886626.787+499514.052i (158 34 47.1)  -1800 -1800 #f)
    #(53930.645 2886228.571+499731.729i (144 5  43.6)  -1800  0    210)
    #(54140.645 2886063.414+499861.382i (140 45 11.5)  0      0    #f)
    ))