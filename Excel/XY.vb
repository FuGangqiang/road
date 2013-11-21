Option Explicit

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

Public Function fs(x0 As Double, y0 As Double, x1 As Double, y1 As Double)
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

Public Function xy_r0r0(x0 As Double, y0 As Double, rad As Double, length As Double)
    Dim z As Variant
    z = zs(x0, y0, rad, length)
    xy_r0r0 = Array(z(0), z(1), rad)
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

Private Function xy(L As Double, file As String)
    Dim l0 As Double
    Dim x0 As Double, y0 As Double
    Dim d As Double, m As Double, s As Double
    Dim r1 As Double, r2 As Double
    Dim lh As Double
    Dim z As Variant
    Dim r As Long
    r = find_l_in_file(L, file)
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
        z = xy_r0r0(x0, y0, dms2rad(d, m, s), L - l0)
    ElseIf r1 = r2 Then '圆曲线
        z = xy_rnrn(x0, y0, dms2rad(d, m, s), L - l0, r1)
    ElseIf r1 = 0 Then '正向完整缓和曲线
        z = xy_r0rn(x0, y0, dms2rad(d, m, s), L - l0, r2, lh)
    ElseIf r2 = 0 Then '反向完整缓和曲线
        With Worksheets(file)
            l0 = .Cells(r + 1, 1)
            x0 = .Cells(r + 1, 2)
            y0 = .Cells(r + 1, 3)
            d = .Cells(r + 1, 4)
            m = .Cells(r + 1, 5)
            s = .Cells(r + 1, 6)
        End With
        z = xy_r0rn(x0, y0, dms2rad(d, m, s) + pi, l0 - L, -r1, lh)
        z = Array(z(0), z(1), z(2) + pi)
    Else
        MsgBox "不完整缓和曲线还没完成编写"
    End If
    xy = z
End Function

Public Function yxy(L As Double, Optional s As Double)
    Dim z0 As Variant
    Dim x0 As Double
    Dim y0 As Double
    Dim rad0 As Double
    z0 = xy(L, "YXY")
    If IsMissing(s) Then '支距为0
        yxy = z0
    Else '支距不为0
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

Public Function zxy(L As Double, Optional s As Double)
    Dim z0 As Variant
    Dim x0 As Double
    Dim y0 As Double
    Dim rad0 As Double
    z0 = xy(L, "ZXY")
    If IsMissing(s) Then '支距为0
        zxy = z0
    Else '支距不为0
        x0 = z0(0)
        y0 = z0(1)
        rad0 = z0(2)
        If s > 0 Then
            rad0 = rad0 + pi / 2
        Else
            rad0 = rad0 - pi / 2
        End If
        z0 = zs(x0, y0, rad0, Abs(s))
        zxy = Array(z0(0), z0(1), floor_remainder(rad0, 2 * pi))
    End If
End Function
