#|
    h.sld -- 高程计算模块
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

(define-library (road h)
  (export h zh yh)
  (import (scheme base)
          (scheme load))
  (include "data.scm")
  (include "h.scm"))
