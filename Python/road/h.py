# h.py -- 高程计算
# Copyright (c) 2013 Fu Gangqiang. All right reserved.
# BSD-style license

from .data import h as data

def h(l, lines):
    '高程计算'
    for i in range(len(lines)-1):
        if lines[i][0] <= l <= lines[i+1][0]:
            break
    assert lines[i][0] <= l <= lines[i+1][0], "l({}): Out of range".format(l)
    L1, H1, R1, T1, I1 = lines[i]
    L2, H2, R2, T2, I2 = lines[i+1]
    g = H1 + I1*(l-L1)
    def f(x, r):
        '计算标高修正值'
        return x**2 / (2*r)
    if L1 <= l <= L1+T1:
         g += f(L1+T1-l, R1)
    elif L2-T2 <= l <= L2:
         g += f(l-(L2-T2), R2)
    return g

def zh(l):
    '左线高程计算'
    return h(l, data['z'])

def yh(l):
    '右线高程计算'
    return h(l, data['y'])
