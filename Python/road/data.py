# data.py -- 坐标高程数据
# Copyright (c) 2013 Fu Gangqiang. All right reserved.
# BSD-style license

# 其中 data 是一个由数个元组成的列表，每一个元组就是一个线元（直线、圆曲线或缓和曲线）

# 高程线元数据
# 线元数据中，若半径为正，则曲线为凹曲线；若半径为负，则曲线为凸曲线。
# (里程 高程 曲线半径 切线长 坡度)
# (L H R T I)
h ={
    #右线
    'y':[(46450, 665.970, 80000,  200,   0.03),
         (47550, 698.970, -85000, 212.5, 0.025),
         (48700, 727.720, 80000,  200,   0.03),
         (49800, 760.720, -35000, 227.5, 0.017)],
    #左线
    'z':[(46450, 665.570, 80000,  200,   0.03),
         (47550, 698.570, -85000, 212.5, 0.025),
         (48750, 728.570, 80000,  200,   0.03),
         (49850, 761.570, -35000, 227.5, 0.017)]
}

# 坐标线元数据
# 线元数据中，若半径为正，则曲线向右转；若半径为负，则曲线向左转。
# (里程 坐标 方位角(d m s) 起点半径 终点半径 缓和曲线长度)
# (L xy dms r1 r2 ls)
xy = {
    #右线
    'y':[(45700.089, 2894117.556+500744.038j, (194, 17, 39.6),  0,    0,      False), # 直线
         (47055.633, 2892803.979+500409.351j, (194, 17, 39.6),  0,     -1480, 165),   # 正向完整缓和曲线
         (47220.633, 2892643.381+500371.595j, (191,  6, 1.73),  -1480, -1480, False), # 圆曲线
         (47871.792, 2891997.684+500388.546j, (165, 53, 30.93), -1480, 0,     165),   # 反向完整缓和曲线
         (48036.792, 2891839.287+500434.676j, (162, 41, 53),    0,     0,     False), # 直线
         (48291.984, 2891595.643+500510.571j, (162, 41, 53),    0,     800,   145),   # 正向完整缓和曲线
         (48436.984, 2891456.015+500549.480j, (167, 53, 25.75), 800,   800,   False), # 圆曲线
         (48957.197, 2890948.224+500491.452j, (205,  8, 52.79), 800,   0,     145),   # 反向完整缓和曲线
         (49102.197, 2890820.975+500422.046j, (210, 20, 25.5),  0,     0,     False), # 直线
         (49510.315, 2890468.753+500215.891j, (210, 20, 25.5),  0,     -1000, 140),   # 正向完整缓和曲线
         (49650.315, 2890346.337+500148.025j, (206, 19, 46.96), -1000, -1000, False), # 圆曲线
         (49909.924, 2890101.402+500064.201j, (191, 27, 18.76), -1000, 0,     140)],  # 反向完整缓和曲线
    #左线
    'z':[(45871.346, 2893966.911+500733.518j, (194, 17, 39.6),  0,     0,     False), # 直线
         (47089.759, 2892786.220+500432.688j, (194, 17, 39.6),  0,     -1480, 165),   # 正向完整缓和曲线
         (47254.759, 2892625.621+500394.932j, (191, 6, 1.73),   -1480, -1480, False), # 圆曲线
         (47905.918, 2891979.924+500411.883j, (165, 53, 30.93), -1480, 0,     165),   # 反向完整缓和曲线
         (48070.918, 2891821.528+500458.013j, (162, 41, 53),    0,     0,     False), # 直线
         (48332.968, 2891571.335+500535.949j, (162, 41, 53),    0,     800,   145),   # 正向完整缓和曲线
         (48477.968, 2891431.708+500574.858j, (167, 53, 25.75), 800,   800,   False), # 圆曲线
         (48915.797, 2891000.161+500547.941j, (199, 14, 51.64), 800,   0,     145),   # 反向完整缓和曲线
         (49060.797, 2890866.451+500491.983j, (204, 26, 24.4),  0,     0,     False), # 直线
         (49556.014, 2890415.608+500287.091j, (204, 26, 24.4),  -2500, -2500, False), # 圆曲线
         (50236.938, 2889765.204+500092.736j, (188, 50, 4.14),  0,     0,     False)] # 直线
}


# 横坡线元数据
# 线元数据中，若横坡为正，则路面自左向右为上坡；若横坡为负，则路面自左向右为下坡。
# (里程, 横坡)
# (L, I)
i = {
    # 右线
    'y':[(45385, -0.02),
         (47065, -0.02),
         (47155,  0.02),
         (47940,  0.02),
         (48030, -0.02),
         (48340, -0.02),
         (48415, -0.04),
         (48980, -0.04),
         (49055, -0.02),
         (49510, -0.02),
         (49600,  0.02),
         (49625,  0.03),
         (49935,  0.03)],
    # 左线
    'z':[(45765,  0.02),
         (48335,  0.02),
         (48425, -0.02),
         (48470, -0.04),
         (48925, -0.04),
         (48970, -0.02),
         (49060,  0.02),
         (52710,  0.02)]
}

# 边坡线元数据
# 线元数据中，若坡率为正，则为上坡；若坡率为负，则为下坡。
# (横向线元长度, 线元坡率)
# (ds, i)
# 线元是从设计线往一侧延伸的
# bp['y']['z']['w'] 索引右线左侧挖方边坡线元
# bp[左右线][左右侧][填挖]
bp = {
    # 左线
    'z':{
        # 左侧
        # 右侧
        },
    # 右线
    'y':{
        # 左侧
        'z':{
            # 挖方
            'w': [(0.25,  0.02),
                  (0.75, -0.04),
                  (0.8,   0),
                  (1,     0.04),
                  (7.5,   1/0.75),
                  (2,     0.04),
                  (10,    1),
                  (2,     0.04),
                  (10,    1),
                  (2,     0.04),
                  (99,    1/1.25)],
            # 填方
            't': [(0.25, 0.02),
                  (0.75, -0.04)]},
            # 右侧
        'y':{}
    }
}

# 加宽带线元数据
