#|
    h.scm -- 高程计算
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

;; for R5RS
;; 加载 data.scm 中的高程数据
;; *data-h-z* 左线高程数据
;; *data-h-y* 右线高程数据
;; (load "data.scm")

(define (h l data)
  ;; 全线高程计算
  ;; l 为计算里程，data 为高程数据
  ;; data 为一 list，其元素类型为 vector: #(里程L 高程H 曲线半径R 切线长T 坡度I)
  (define (f x r)
    ;; 计算标高修正值：x^2/(2R)
    (/ (* x x)
       (* 2 r)))
  (let loop ((ls data))
    (if (null? (cdr ls))
        (error "Error:out of range")
        (let ((v0 (car ls))
              (v1 (cadr ls)))
          (let ((L0 (vector-ref v0 0))
                (H0 (vector-ref v0 1))
                (R0 (vector-ref v0 2))
                (T0 (vector-ref v0 3))
                (I0 (vector-ref v0 4))
                (L1 (vector-ref v1 0))
                (H1 (vector-ref v1 1))
                (R1 (vector-ref v1 2))
                (T1 (vector-ref v1 3))
                (I1 (vector-ref v1 4)))
            (if (<= L0 l L1)
                (let ((g (+ H0 (* I0 (- l L0)))))   ; 先按照直线来计算标高，然后判断是否需要修正值
                  (cond ((<= L0 l (+ L0 T0))
                         (+ g (f (- (+ L0 T0) l)
                                 R0)))              ; 曲线段，有修正值
                        ((< (+ L0 T0) l (- L1 T1))
                         g)                         ; 直线段，无修正值
                        ((<= (- L1 T1) l L1)
                         (+ g (f (- l (- L1 T1))
                                 R1)))              ; 曲线段，有修正值
                        (else
                         (error "Error:can not be here"))))
                (loop (cdr ls))))))))

(define (zh l)
  ;; 左线高程计算
  ;; l 为计算里程
  (h l *data-h-z*))

(define (yh l)
  ;; 右线高程计算
  ;; l 为计算里程
  (h l *data-h-y*))
