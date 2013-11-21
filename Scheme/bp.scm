#|
bp.scm -- 边坡超欠挖计算
Copyright (c) 2012 Fu Gangqiang. All right reserved.
BSD-style license
|#

(define *data-bp*
  ; (边坡横向长度  边坡坡率)
  '((0.25        2/100)     ; 路面横坡
    (0.75        -4/100)    ; 路肩
    (0.8         0)         ; 边沟
    (1           4/100)     ; 边坡碎落台
    (7.5         100/75)    ; 一级边坡 1:0.75
    (2           4/100)     ; 一级平台
    (10          1)         ; 二级边坡 1:1
    (2           4/100)     ; 二级平台
    (10          1)         ; 三级边坡 1:1
    (2           4/100)     ; 三级平台
    (99          100/125)   ; 四级边坡 1:1.25
    ))

(define (H^ S0 H0 i S)
  (+ H0 (/ (- S S0)
           i)))

(define (S^ S0 H0 i H)
  (+ S0 (* (- H H0)
           i)))

;; 寻找边坡起坡点坐标及相应坡率
;; H 为放样点标高与其里程路面设计标高之差
;; data 为边坡元数据
(define (find-bp H)
  (let loop ((S0 0) (H0 0) (ls *data-bp*))
    (let* ((ds (car (car ls)))
           (i (cadr (car ls)))
           (dh (* ds i)))
      (if (<= H (+ H0 dh 0.000001))
          (list S0 H0 i)
          (if (null? (cdr ls))
              (error "Out of range H" H)
              (loop (+ S0 ds)
                    (+ H0 dh)
                    (cdr ls)))))))

;; 计算边坡超欠挖
;; (S H) 表示放样点坐标
(define (cqw S H)
  (let* ((shi (find-bp H))
         (S0 (car shi))
         (H0 (cadr shi))
         (i (car (cddr shi))))
    (let ((ds (- (S^ S0 H0 i H)   ; 水平超欠挖(正欠负超)
                 S))
          (dh (- (H^ S0 H0 i S)   ; 竖直超欠挖(正超负欠)
                 H)))
      (list ds dh))))
