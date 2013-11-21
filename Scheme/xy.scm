#|
    xy.scm -- 坐标正反算
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

;; 坐标运算运用复数
;; 加载 data.scm 中的坐标数据
;; *data-xy-z* 左线高程数据
;; *data-xy-y* 右线高程数据
;; (load "data.scm")

(define pi 3.141592653589793)
(define tolerance 0.00000001)

(define (degree->radian d-m-s)
  ;; 转换角度单位为弧度单位
  ;; d-m-s 为 (d m s) 形式列表
  (let* ((d (car d-m-s))
         (m-s (cdr d-m-s))
         (m (car m-s))
         (s (cadr m-s)))
    (let* ((d (floor-remainder d 360))
           (deg (+ d (/ m 60.0) (/ s 3600))))
      (* pi (/ deg 180.0)))))

(define (radian->degree rad)
  ;; 转换弧度单位为角度单位
  ;; rad 为数值
  (let* ((rad (floor-remainder rad (* 2 pi)))
         (deg (* 180 (/ rad pi))))
    (let* ((d  (truncate deg))
           (deg (* 60 (- deg d)))
           (m (truncate deg))
           (s (* 60 (- deg m))))
      (list d m s))))

(define (cross* p0 p1 p2)
  ;; 叉积
  ;; p0, p1, p2 为复数坐标形式
  ;; 返回值 < 0: 说明三点在 p1 右转
  ;;       = 0: 说明三点共线
  ;;       > 0: 说明三点在 p1 左转
  (let ((a1 (- p1 p0))
        (a2 (- p2 p0)))
    (let ((x1 (real-part a1))
          (y1 (imag-part a1))
          (x2 (real-part a2))
          (y2 (imag-part a2)))
      (- (* x1 y2)
         (* x2 y1)))))

(define (zs-r0r0 point len)
  ;; 直线线元正算
  ;; point 为 (z . rad) 形式， (线元起点坐标 . 线元起点方位角)
  ;; len 为计算前进长度
  ;; 返回 (z . rad),同 point
  (let ((z (car point))
        (rad (cdr point)))
    (cons (+ z (make-polar len rad))
          rad)))

(define (fs-r0r0 z point len)
  ;; 直线线元反算
  ;; z 为计算坐标
  ;; point 为线元起点坐标
  ;; len 为线元长度
  ;; 返回 (l . s) 形式

  (define (distance l)
    ;; 返回 z 点与直线线元起点前进 l 长度后的点的距离
    (magnitude (- (car (zs-r0r0 point l))
                  z)))

  (let loop ((start 0) (end len))
    (let ((mid (/ (+ start end)
                     2)))
      (if (< (- end start)
             tolerance)
          (let ((p0 (car (zs-r0r0 point start)))
                (p1 (car (zs-r0r0 point end)))
                (s (distance mid)))
            (cons (inexact mid)
                  (if (< (cross* p0 p1 z)
                         0)
                      (- s)
                      s)))
          (if (< (distance start)
                 (distance end))
              (loop start mid)
              (loop mid end))))))

(define (zs-rnrn point r len)
  ;; 圆曲线线元正算
  ;; point 为 (z . rad) 形式， (线元起点坐标 . 线元起点方位角)
  ;; len 为计算前进长度
  ;; r 为圆曲线线元半径
  ;; 返回 (z . rad),同 point
  (let ((z (car point))
        (rad (cdr point)))
    (let* ((drad (/ len r 2))
           (dlen (abs (* 2 r (sin drad)))))
      (cons (+ z (make-polar dlen (+ rad drad)))
            (+ rad (* 2 drad))))))

(define (fs-rnrn z point r len)
  ;; 圆曲线线元反算
  ;; z 为计算坐标
  ;; point 为圆曲线线元起点坐标
  ;; r 为圆曲线线元半径
  ;; len 为线元长度
  ;; 返回 (l . s) 形式

  (define (distance l)
    ;; 返回 z 点与圆曲线线元起点前进 l 长度后的点的距离
    (magnitude (- (car (zs-rnrn point r l))
                  z)))

  (let loop ((start 0) (end len))
    (let ((mid (/ (+ start end)
                     2)))
      (if (< (- end start)
             tolerance)
          (let ((p0 (car (zs-rnrn point r start)))
                (p1 (car (zs-rnrn point r end)))
                (s (distance mid)))
            (cons (inexact mid)
                  (if (< (cross* p0 p1 z)
                         0)
                      (- s)
                      s)))
          (if (< (distance start)
                 (distance end))
              (loop start mid)
              (loop mid end))))))

(define (zs-r0rn point r lh len)
  ;; 缓和曲线线元正算
  ;; point 为 (z . rad) 形式， (线元起点坐标 . 线元起点方位角)
  ;; len 为计算前进长度
  ;; r 为缓和曲线线元终点半径
  ;; lh 为缓和曲线长度
  ;; 返回 (z . rad),同 point
  (let ((z (car point))
        (rad (cdr point)))
    (let ((A (* r lh)))
      (define (f n m)
        (* (expt -1 (/ n 2))
           (/ (expt len (+ 1 (* 2 n)))
              (* m (expt A n)))))
      (let* ((drad (/ (expt len 2) (* 6 A)))
             (dlen (/ (+ len
                         (f 2 40)
                         (f 4 3456)
                         (f 6 599040)
                         (f 8 175472640))
                      (cos (abs drad)))))
        (cons (+ z (make-polar dlen (+ rad drad)))
              (+ rad (* 3 drad)))))))

(define (fs-r0rn z point r lh len)
  ;; 缓和曲线线元反算
  ;; z 为计算坐标
  ;; point 为缓和曲线线元起点坐标
  ;; r 为缓和曲线线元半径
  ;; lh 为缓和曲线线元长度
  ;; len 为缓和曲线线元长度
  ;; 返回 (l . s) 形式

  (define (distance l)
    ;; 返回 z 点与缓和曲线线元起点前进 l 长度后的点的距离
    (magnitude (- (car (zs-r0rn point r lh l))
                  z)))

  (let loop ((start 0) (end len))
    (let ((mid (/ (+ start end)
                     2)))
      (if (< (- end start)
             tolerance)
          (let ((p0 (car (zs-r0rn point r lh start)))
                (p1 (car (zs-r0rn point r lh end)))
                (s (distance mid)))
            ;; (cons (exact->inexact mid)
            (cons (inexact mid)
                  (if (< (cross* p0 p1 z)
                         0)
                      (- s)
                      s)))
          (if (< (distance start)
                 (distance end))
              (loop start mid)
              (loop mid end))))))

(define (find-in-between l data)
  (let loop ((xs data))
    (if (null? (cdr xs))
        (error "Out of range: L = " l)
        (let ((x1 (car xs))
              (x2 (cadr xs)))
          (let ((l1 (vector-ref x1 0))
                (l2 (vector-ref x2 0)))
            (if (<= l1 l l2)
                (cons x1 x2)
                (loop (cdr xs))))))))

(define (xy l data)
  (let ((vs (find-in-between l data)))
    (let ((v1 (car vs))
          (v2 (cdr vs)))
      (let ((l1 (vector-ref v1 0))
            (z1 (vector-ref v1 1))
            (rad1 (degree->radian (vector-ref v1 2)))
            (r11 (vector-ref v1 3))
            (r12 (vector-ref v1 4))
            (lh1 (vector-ref v1 5))
            (l2 (vector-ref v2 0))
            (z2 (vector-ref v2 1))
            (rad2 (degree->radian (vector-ref v2 2)))
            (r21 (vector-ref v2 3))
            (r22 (vector-ref v2 4))
            (lh2 (vector-ref v2 5)))
        (cond ((= 0 r11 r12)            ; 直线
               (zs-r0r0 (cons z1 rad1)
                        (- l l1)))
              ((= r11 r12)              ; 圆曲线
               (zs-rnrn (cons z1 rad1)
                        r11
                        (- l l1)))
              ((= r11 0)                ; 正向缓和曲线
               (zs-r0rn (cons z1 rad1)
                        r12
                        lh1
                        (- l l1)))
              ((= r12 0)                ; 反向缓和曲线
               (let ((point (zs-r0rn (cons z2 (+ rad2 pi))
                                     (- r11)
                                     lh1
                                     (- l2 l))))
                 (cons (car point)
                       (+ pi (cdr point)))))
              (else
               (error "Unknow line type")))))))

(define (zxy l s)
  (let* ((point (xy l *data-xy-z*)))
    (if (= s 0)
        point
        (let ((z (car point))
              (rad (cdr point)))
          (if (> s 0)
              (zs-r0r0 (cons z
                             (+ rad (/ pi 2)))
                       s)
              (zs-r0r0 (cons z
                             (+ rad (- (/ pi 2))))
                       (abs s)))))))

(define (yxy l s)
  (let* ((point (xy l *data-xy-y*)))
    (if (= s 0)
        point
        (let ((z (car point))
              (rad (cdr point)))
          (if (> s 0)
              (zs-r0r0 (cons z
                             (+ rad (/ pi 2)))
                       s)
              (zs-r0r0 (cons z
                             (+ rad (- (/ pi 2))))
                       (abs s)))))))

(define (ls guess z data)
  (let ((vs (find-in-between guess data)))
    (let ((v1 (car vs))
          (v2 (cdr vs)))
      (let ((l1 (vector-ref v1 0))
            (z1 (vector-ref v1 1))
            (rad1 (degree->radian (vector-ref v1 2)))
            (r11 (vector-ref v1 3))
            (r12 (vector-ref v1 4))
            (lh1 (vector-ref v1 5))
            (l2 (vector-ref v2 0))
            (z2 (vector-ref v2 1))
            (rad2 (degree->radian (vector-ref v2 2)))
            (r21 (vector-ref v2 3))
            (r22 (vector-ref v2 4))
            (lh2 (vector-ref v2 5)))
        (cond ((= 0 r11 r12)            ; 直线
               (let* ((uv (fs-r0r0 z
                                   (cons z1 rad1)
                                   (- l2 l1)))
                      (u (car uv))
                      (v (cdr uv)))
                 (cons (+ l1 u)
                       v)))
              ((= r11 r12)              ; 圆曲线
               (let* ((uv (fs-rnrn z
                                   (cons z1 rad1)
                                   r11
                                   (- l2 l1)))
                      (u (car uv))
                      (v (cdr uv)))
                 (cons (+ l1 u)
                       v)))
              ((= r11 0)                ; 正向缓和曲线
               (let* ((uv (fs-r0rn z
                                   (cons z1 rad1)
                                   r12
                                   lh1
                                   (- l2 l1)))
                      (u (car uv))
                      (v (cdr uv)))
                 (cons (+ l1 u)
                       v)))
              ((= r12 0)                ; 反向缓和曲线
               (let* ((uv (fs-r0rn z
                                   (cons z2 (+ rad2 pi))
                                   (- r11)
                                   lh1
                                   (- l2 l1)))
                      (u (car uv))
                      (v (cdr uv)))
                 (cons (- l2 u)
                       (- v))))
              (else
               (error "Unknow line type")))))))

(define (zls guess z)
  (ls guess z *data-xy-z*))


(define (yls guess z)
  (ls guess z *data-xy-y*))
