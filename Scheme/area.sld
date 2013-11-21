#|
    area.sld -- 面积计算模块
    Copyright (c) 2011,2012 Fu Gangqiang. All right reserved.
    BSD-style license
|#

(define-library (road area)
  (export polygon)
  (import (scheme base)
          (scheme complex))
  (include "area.scm"))
