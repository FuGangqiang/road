#|
    polygon.scm -- 面积计算
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#


(define (polygon points)
  ;; points 为一个 list，元素形式为 x+yi <复数>
  ;; polygon 函数结果：(x1y2-x2y1 + x2y3-x3y2 + ... + xny1-x1yn)/2
  (define (f p1 p2)
    ;;辅助函数，结果为：x1y2-x2y1
    (- (* (real-part p1)
          (imag-part p2))
       (* (real-part p2)
          (imag-part p1))))

  (let ((first (car points)))
    (let loop ((sum 0) (ps points))
      (if (null? (cdr ps))
          (/ (+ sum
                (f (car ps) first))
             2.0)
          (loop (+ sum (f (car ps)
                          (cadr ps)))
                (cdr ps))))))
