# xy.py -- 坐标正反算
# Copyright (c) 2013 Fu Gangqiang. All right reserved.
# BSD-style license

from math import pi, cos, sin, radians, copysign
from cmath import rect
from .data import xy as data

tolerance = 1e-4

def cross_prod(p0, p1, p2):
    '''叉积
    p0, p1, p2 为复数坐标形式
    返回值 < 0: 说明三点在 p1 右转
          = 0: 说明三点共线
          > 0: 说明三点在 p1 左转
    '''
    a1 = p1 - p0
    a2 = p2 -p0
    return a1.real*a2.imag - a2.real*a1.imag

# point 为 (x+yi, rad) 形式,(线元起点坐标 . 线元起点方位角)
# len 在正算中为计算前进长度,在反算中为线元长度
# z 为坐标复数形式
# 正算返回 (x+yi, rad) 形式,同 point
# 反算返回 (l, s) 形式

def zs_r0r0(point, len):
    '直线线元正算'
    z, rad = point
    return (z+rect(len, rad),
            rad)

def fs_r0r0(z, point, len):
    '直线线元反算'
    def distance(l):
        '返回 z 点与直线线元起点前进 l 长度后的点的距离'
        zl, radl = zs_r0r0(point, l)
        return abs(z-zl)
    start, end = 0, len
    while abs(start-end) > tolerance:
        if distance(start) > distance(end):
            start = (start+end)/2
        else:
            end = (start+end)/2
    p0, rad0 = zs_r0r0(point, start)
    p1, rad1 = zs_r0r0(point, end)
    l = (start+end)/2
    s = copysign(distance(l), cross_prod(p0, p1, z))
    return (l, s)

def zs_rnrn(point, r, len):
    '圆曲线线元正算'
    z, rad = point
    drad = len/(2*r)
    dlen = abs(2*r*sin(drad))
    return (z+rect(dlen, rad+drad),
            rad+2*drad)

def fs_rnrn(z, point, r, len):
    '圆曲线线元反算'
    def distance(l):
        '返回 z 点与直线线元起点前进 l 长度后的点的距离'
        zl, radl = zs_rnrn(point, r, l)
        return abs(z-zl)
    start, end = 0, len
    while abs(start-end) > tolerance:
        if distance(start) > distance(end):
            start = (start+end)/2
        else:
            end = (start+end)/2
    p0, rad0 = zs_rnrn(point, r, start)
    p1, rad1 = zs_rnrn(point, r, end)
    l = (start+end)/2
    s = copysign(distance(l), cross_prod(p0, p1, z))
    return (l, s)

def zs_r0rn(point, r, lh, len):
    '正向完整缓和曲线正算'
    z, rad = point
    A = r * lh
    def f(n, m):
        return (-1)**(n/2) * len**(1+2*n) / (m * A**n)
    drad = len**2 / (6*A)
    dlen = (len + f(2, 40) + f(4, 3456) + f(6, 599040) + f(8, 175472640)) / cos(abs(drad))
    return (z+rect(dlen, rad+drad),
            rad+3*drad)

def fs_r0rn(z, point, r, lh, len):
    '正向完整缓和曲线线元反算'
    def distance(l):
        '返回 z 点与直线线元起点前进 l 长度后的点的距离'
        zl, radl = zs_r0rn(point, r, lh, l)
        return abs(z-zl)
    start, end = 0, len
    while abs(start-end) > tolerance:
        if distance(start) > distance(end):
            start = (start+end)/2
        else:
            end = (start+end)/2
    p0, rad0 = zs_r0rn(point, r, lh, start)
    p1, rad1 = zs_r0rn(point, r, lh, end)
    l = (start+end)/2
    s = copysign(distance(l), cross_prod(p0, p1, z))
    return (l, s)

def xy(l, lines):
    for i in range(len(lines)-1):
        if lines[i][0] <= l <= lines[i+1][0]:
            break
    assert lines[i][0] <= l <= lines[i+1][0], "l({}): Out of range".format(l)
    L1, z1, (d1, m1, s1), r11, r12, lh1 = lines[i]
    L2, z2, (d2, m2, s2), r21, r22, lh2 = lines[i+1]
    rad1 = radians(d1%360 + m1/60 + s1/3600)
    rad2 = radians(d2%360 + m2/60 + s2/3600)
    if r11 == r12 == 0:         # 直线
        return zs_r0r0((z1, rad1), l-L1)
    elif r11 == r12:            # 圆曲线
        return zs_rnrn((z1, rad1), r11, l-L1)
    elif r11 == 0:              # 正向完整缓和曲线
        return zs_r0rn((z1, rad1), r12, lh1, l-L1)
    elif r12 == 0:              # 反向完整缓和曲线
        z, rad = zs_r0rn((z2, rad2+pi), -r11, lh1, L2-l)
        return (z, rad+pi)
    else:                       # todo 不完整缓和曲线
        raise Exception('Unkow line type')

def zxy(l, s=0):
    z, rad = xy(l, data['z'])
    if s == 0:
        return (z, rad)
    else:
        return zs_r0r0((z, rad+copysign(pi/2, s)), abs(s))

def yxy(l, s=0):
    z, rad = xy(l, data['y'])
    if s == 0:
        return (z, rad)
    else:
        return zs_r0r0((z, rad+copysign(pi/2, s)), abs(s))

def turn(point, t):
    z, rad = point
    for drad, len in t:
        z, rad = zs_r0r0((z, rad+drad), len)
    return (z, rad)

def ls(guess, z, lines):
    for i in range(len(lines)-1):
        if lines[i][0] <= guess <= lines[i+1][0]:
            break
    assert lines[i][0] <= guess <= lines[i+1][0], "Guess({}): Out of range".format(guess)
    L1, z1, (d1, m1, s1), r11, r12, lh1 = lines[i]
    L2, z2, (d2, m2, s2), r21, r22, lh2 = lines[i+1]
    rad1 = radians(d1%360 + m1/60 + s1/3600)
    rad2 = radians(d2%360 + m2/60 + s2/3600)
    if r11 == r12 == 0:         # 直线
        dl, s = fs_r0r0(z, (z1, rad1), L2-L1)
        return (L1+dl, s)
    elif r11 == r12:            # 圆曲线
        dl, s = fs_rnrn(z, (z1, rad1), r11, L2-L1)
        return (L1+dl, s)
    elif r11== 0:               # 正向完整缓和曲线
        dl, s = fs_r0rn(z, (z1, rad1), r12, lh1, L2-L1)
        return (L1+dl, s)
    elif r12 == 0:              # 反向完整缓和曲线
        dl, s = fs_r0rn(z, (z2, rad2+pi), -r11, lh1, L2-L1)
        return (L2-dl, -s)
    else:                       # todo 不完整缓和曲线
        raise Exception('Unkow line type')

def zls(guess, z):
    return ls(guess, z, data['z'])

def yls(guess, z):
    return ls(guess, z, data['y'])
