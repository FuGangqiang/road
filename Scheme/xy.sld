#|
    xy.sld -- 坐标正反算库
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

(define-library (road xy)
  (export xy zxy yxy
          ls zls yls
          zs-r0r0 zs-rnrn zs-r0rn
          fs-r0r0 fs-rnrn fs-r0rn)
  (import (scheme base)
          (scheme inexact)
          (scheme division)
          (scheme complex))
  (include "data13.scm")
  (include "xy.scm"))
