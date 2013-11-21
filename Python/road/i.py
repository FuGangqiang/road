# i.py -- 横坡计算
# Copyright (c) 2013 Fu Gangqiang. All right reserved.
# BSD-style license

from .data import i as data

def i(l, lines):
    '横坡计算'
    for n in range(len(lines)-1):
        if lines[n][0] <= l <= lines[n+1][0]:
            break
    assert lines[n][0] <= l <= lines[n+1][0], "l({}): Out of range".format(l)
    L1, I1 = lines[n]
    L2, I2 = lines[n+1]
    Lc = L2 - L1
    if I1 == I2:                # 无超高
        return I1
    else:                       # 有超高
        dl = l - L1
        return  I1 + (I2 - I1) * (dl**2/Lc**2) * (3-2*dl/Lc) # 三次抛物线公式

def zi(l):
    return i(l, data['z'])

def yi(l):
    return i(l, data['y'])
