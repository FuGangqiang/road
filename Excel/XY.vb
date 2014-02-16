Option Explicit

' 用于控制反算精度
Public Const tolerance As Double = 0.000001

Function floor_remainder(x As Double, y As Double) As Double
    floor_remainder = x - Int(x / y) * y
End Function

Public Function dms2rad(d As Double, m As Double, s As Double) As Double
    Dim dms As Double
    d = floor_remainder(d, 360)
    dms = d + m / 60 + s / 3600
    dms2rad = dms / 180 * pi
End Function

Public Function rad2dms(rad As Double)
    Dim d As Double
    Dim m As Double
    Dim s As Double
    Dim dms As Double
    rad = floor_remainder(rad, 2 * pi)
    dms = rad * 180 / pi
    d = Int(dms)
    dms = (dms - d) * 60
    m = Int(dms)
    dms = (dms - m) * 60
    s = dms
    rad2dms = Array(d, m, s)
End Function

Public Function zs(x0 As Double, y0 As Double, rad As Double, length As Double)
    Dim x1 As Double
    Dim y1 As Double
    x1 = x0 + length * Cos(rad)
    y1 = y0 + length * Sin(rad)
    zs = Array(x1, y1)
End Function

Public Function fs(x0 As Double, y0 As Double, x1 As Variant, y1 As Variant)
    Dim length As Double
    Dim rad As Double
    Dim dx As Double
    Dim dy As Double
    dx = x1 - x0
    dy = y1 - y0
    length = Sqr(dx ^ 2 + dy ^ 2)
    rad = WorksheetFunction.Atan2(dx, dy)
    rad = floor_remainder(rad, 2 * pi)
    fs = Array(rad, length)
End Function

Public Function cross_prod(x0 As Variant, y0 As Variant, x1 As Variant, y1 As Variant, x2 As Variant, y2 As Variant)
    Dim x01 As Double, y01 As Double, x02 As Double, y02 As Double
    x01 = x1 - x0
    y01 = y1 - y0
    x02 = x2 - x0
    y02 = y2 - y0
    cross_prod = x01 * y02 - x02 * y01
End Function

Public Function pj(rad0 As Double, rad1 As Double)
    ' 偏角
    pj = floor_remainder(rad0 + rad1, 2 * pi)
End Function

Public Function xy_r0r0(x0 As Double, y0 As Double, rad As Double, length As Double)
    Dim z As Variant
    z = zs(x0, y0, rad, length)
    xy_r0r0 = Array(z(0), z(1), rad)
End Function

Public Function distance_r0r0(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double)
    Dim z As Variant
    Dim dz As Variant
    z = xy_r0r0(x0, y0, rad, length)
    dz = fs(x, y, z(0), z(1))
    distance_r0r0 = dz(1)
End Function

Public Function ls_r0r0(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double)
    Dim l0 As Double, l1 As Double
    Dim z0 As Variant, z1 As Variant
    Dim l As Double, s As Double
    l0 = 0
    l1 = length
    While Abs(l0 - l1) > tolerance
        If distance_r0r0(x, y, x0, y0, rad, l0) > distance_r0r0(x, y, x0, y0, rad, l1) Then
            l0 = (l0 + l1) / 2
        Else
            l1 = (l0 + l1) / 2
        End If
    Wend
    z0 = xy_r0r0(x0, y0, rad, l0)
    z1 = xy_r0r0(x0, y0, rad, l1)
    l = (l0 + l1) / 2
    s = distance_r0r0(x, y, x0, y0, rad, l)
    s = VBA.Sgn(cross_prod(z0(0), z0(1), z1(0), z1(1), x, y)) * Abs(s)
    ls_r0r0 = Array(l, s)
End Function

Public Function xy_rnrn(x0 As Double, y0 As Double, rad As Double, length As Double, r As Double)
    Dim z As Variant
    Dim drad As Double
    Dim dlength As Double
    drad = length / (2 * r)
    dlength = Abs(2 * r * Sin(drad))
    z = zs(x0, y0, rad + drad, dlength)
    xy_rnrn = Array(z(0), z(1), floor_remainder(rad + 2 * drad, 2 * pi))
End Function

Public Function distance_rnrn(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double, r As Double)
    Dim z As Variant
    Dim dz As Variant
    z = xy_rnrn(x0, y0, rad, length, r)
    dz = fs(x, y, z(0), z(1))
    distance_rnrn = dz(1)
End Function

Public Function ls_rnrn(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double, r As Double)
    Dim l0 As Double, l1 As Double
    Dim z0 As Variant, z1 As Variant
    Dim l As Double, s As Double
    l0 = 0
    l1 = length
    While Abs(l0 - l1) > tolerance
        If distance_rnrn(x, y, x0, y0, rad, l0, r) > distance_rnrn(x, y, x0, y0, rad, l1, r) Then
            l0 = (l0 + l1) / 2
        Else
            l1 = (l0 + l1) / 2
        End If
    Wend
    z0 = xy_rnrn(x0, y0, rad, l0, r)
    z1 = xy_rnrn(x0, y0, rad, l1, r)
    l = (l0 + l1) / 2
    s = distance_rnrn(x, y, x0, y0, rad, l, r)
    s = VBA.Sgn(cross_prod(z0(0), z0(1), z1(0), z1(1), x, y)) * Abs(s)
    ls_rnrn = Array(l, s)
End Function

Private Function f(A As Double, length As Double, n As Long, m As Long) As Double
    f = (-1) ^ (n / 2) * length ^ (2 * n + 1) / (m * A ^ n)
End Function

Public Function xy_r0rn(x0 As Double, y0 As Double, rad As Double, length As Double, r As Double, lh As Double)
    Dim z As Variant
    Dim drad As Double
    Dim dlength As Double
    Dim A As Double
    A = r * lh
    drad = length ^ 2 / (6 * A)
    dlength = (length + f(A, length, 2, 40) _
                      + f(A, length, 4, 3456) _
                      + f(A, length, 6, 599040) _
                      + f(A, length, 8, 175472640)) _
              / Cos(Abs(drad))
    z = zs(x0, y0, rad + drad, dlength)
    xy_r0rn = Array(z(0), z(1), floor_remainder(rad + 3 * drad, 2 * pi))
End Function

Public Function distance_r0rn(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double, r As Double, lh As Double)
    Dim z As Variant
    Dim dz As Variant
    z = xy_r0rn(x0, y0, rad, length, r, lh)
    dz = fs(x, y, z(0), z(1))
    distance_r0rn = dz(1)
End Function


Public Function ls_r0rn(x As Double, y As Double, x0 As Double, y0 As Double, rad As Double, length As Double, r As Double, lh As Double)
    Dim l0 As Double, l1 As Double
    Dim z0 As Variant, z1 As Variant
    Dim l As Double, s As Double
    l0 = 0
    l1 = length
    While Abs(l0 - l1) > tolerance
        If distance_r0rn(x, y, x0, y0, rad, l0, r, lh) > distance_r0rn(x, y, x0, y0, rad, l1, r, lh) Then
            l0 = (l0 + l1) / 2
        Else
            l1 = (l0 + l1) / 2
        End If
    Wend
    z0 = xy_r0rn(x0, y0, rad, l0, r, lh)
    z1 = xy_r0rn(x0, y0, rad, l1, r, lh)
    l = (l0 + l1) / 2
    s = distance_r0rn(x, y, x0, y0, rad, l, r, lh)
    s = VBA.Sgn(cross_prod(z0(0), z0(1), z1(0), z1(1), x, y)) * Abs(s)
    ls_r0rn = Array(l, s)
End Function

Public Function xy(l As Double, file As String)
    Dim l0 As Double, x0 As Double, y0 As Double, d As Double, m As Double, s As Double, r1 As Double, r2 As Double, lh As Double
    Dim z As Variant
    Dim r As Long
    r = find_l_in_file(l, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        x0 = .Cells(r, 2)
        y0 = .Cells(r, 3)
        d = .Cells(r, 4)
        m = .Cells(r, 5)
        s = .Cells(r, 6)
        r1 = .Cells(r, 7)
        r2 = .Cells(r, 8)
        lh = .Cells(r, 9)
    End With
    If r1 = 0 And r2 = 0 Then '直线
        z = xy_r0r0(x0, y0, dms2rad(d, m, s), l - l0)
    ElseIf r1 = r2 Then '圆曲线
        z = xy_rnrn(x0, y0, dms2rad(d, m, s), l - l0, r1)
    ElseIf r1 = 0 Then '正向完整缓和曲线
        z = xy_r0rn(x0, y0, dms2rad(d, m, s), l - l0, r2, lh)
    ElseIf r2 = 0 Then '反向完整缓和曲线
        With Worksheets(file)
            l0 = .Cells(r + 1, 1)
            x0 = .Cells(r + 1, 2)
            y0 = .Cells(r + 1, 3)
            d = .Cells(r + 1, 4)
            m = .Cells(r + 1, 5)
            s = .Cells(r + 1, 6)
        End With
        z = xy_r0rn(x0, y0, dms2rad(d, m, s) + pi, l0 - l, -r1, lh)
        z = Array(z(0), z(1), z(2) + pi)
    Else
        MsgBox "不完整缓和曲线还没完成编写"
    End If
    xy = z
End Function

Public Function yxy(l As Double, Optional s As Variant)
    Dim z0 As Variant
    Dim x0 As Double
    Dim y0 As Double
    Dim rad0 As Double
    z0 = xy(l, "YXY")
    If IsMissing(s) Then
        yxy = z0
    Else
        x0 = z0(0)
        y0 = z0(1)
        rad0 = z0(2)
        If s > 0 Then
            rad0 = rad0 + pi / 2
        Else
            rad0 = rad0 - pi / 2
        End If
        z0 = zs(x0, y0, rad0, Abs(s))
        yxy = Array(z0(0), z0(1), floor_remainder(rad0, 2 * pi))
    End If
End Function

Public Function ls(guess As Double, x As Double, y As Double, file As String)
    Dim l0 As Double, x0 As Double, y0 As Double, d As Double, m As Double, s As Double, r1 As Double, r2 As Double, lh As Double
    Dim length As Double
    Dim l_s As Variant
    Dim r As Long
    r = find_l_in_file(guess, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        x0 = .Cells(r, 2)
        y0 = .Cells(r, 3)
        d = .Cells(r, 4)
        m = .Cells(r, 5)
        s = .Cells(r, 6)
        r1 = .Cells(r, 7)
        r2 = .Cells(r, 8)
        lh = .Cells(r, 9)
        length = Abs(.Cells(r, 1) - .Cells(r + 1, 1))
    End With
    If r1 = 0 And r2 = 0 Then '直线
        l_s = ls_r0r0(x, y, x0, y0, dms2rad(d, m, s), length)
        ls = Array(l0 + l_s(0), l_s(1))
    ElseIf r1 = r2 Then '圆曲线
        l_s = ls_rnrn(x, y, x0, y0, dms2rad(d, m, s), length, r1)
        ls = Array(l0 + l_s(0), l_s(1))
    ElseIf r1 = 0 Then '正向完整缓和曲线
        l_s = ls_r0rn(x, y, x0, y0, dms2rad(d, m, s), length, r2, lh)
        ls = Array(l0 + l_s(0), l_s(1))
    ElseIf r2 = 0 Then '反向完整缓和曲线
        With Worksheets(file)
            l0 = .Cells(r + 1, 1)
            x0 = .Cells(r + 1, 2)
            y0 = .Cells(r + 1, 3)
            d = .Cells(r + 1, 4)
            m = .Cells(r + 1, 5)
            s = .Cells(r + 1, 6)
        End With
        l_s = ls_r0rn(x, y, x0, y0, dms2rad(d, m, s) + pi, length, -r1, lh)
        ls = Array(l0 - l_s(0), -l_s(1))
    Else
        MsgBox "不完整缓和曲线还没完成编写"
    End If
End Function

Public Function yls(guess As Double, x As Double, y As Double)
    yls = ls(guess, x, y, "YXY")
End Function
