# bp.py -- 边坡计算
# Copyright (c) 2013 Fu Gangqiang. All right reserved.
# BSD-style license

from .data import bp as data

def H(S0, H0, i, S):
    return H0 + (S-S0)/i

def S(S0, H0, i, H):
    return S0 + i*(H-H0)

def cqw(S, H):
    pass
