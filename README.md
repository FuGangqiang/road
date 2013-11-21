#road 程序库

一个关于公路测量计算的小程序

四种程序：

1. Casio 5800 程序
2. Scheme(R7RS) 程序库
3. Excel Vba 程序
4. Python 程序

road 库由 (road xy)，(road h)，(road area) 和 (road bp) 等子模块组成，子模块函数如下：

##(road xy)
坐标正算：已知里程[和支距]求坐标

坐标反算：已知坐标求里程和支距

* 正算直线：zs-r0r0
* 正算圆曲线：zs-rnrn
* 正算缓和曲线：zs-r0rn
* 正算全线：xy zxy yxy
* 反算直线：fs-r0rn
* 反算圆曲线：fs-rnrn
* 反算缓和曲线：fs-r0rn
* 反算全线：ls zls yls

##(road h)
由里程计算设计高程

* 全线高程: h
* 左线高程：zh
* 右线高程：yh

##(road area)
多边形面积计算 polygon

##(road bp)
边坡超欠挖计算
