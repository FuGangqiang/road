Option Explicit

Private Function f(x As Double, r As Double) As Double
    '被 H 函数调用
    'r有正负号，向左转为负，向右转为正
    f = (x ^ 2) / (2 * r)
End Function

Private Function H(L As Double, file As String) As Double
    Dim l0 As Double, l1 As Double
    Dim h0 As Double, h1 As Double
    Dim r0 As Double, r1 As Double
    Dim t0 As Double, t1 As Double
    Dim i0 As Double, i1 As Double
    Dim r As Long
    r = find_l_in_file(L, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        h0 = .Cells(r, 2)
        r0 = .Cells(r, 3)
        t0 = .Cells(r, 4)
        i0 = .Cells(r, 5)
        l1 = .Cells(r + 1, 1)
        h1 = .Cells(r + 1, 2)
        r1 = .Cells(r + 1, 3)
        t1 = .Cells(r + 1, 4)
        i1 = .Cells(r + 1, 5)
        H = h0 + i0 * (L - l0)
        If l0 <= L And L <= l0 + t0 Then
            H = H + f(l0 + t0 - L, r0)
        ElseIf l1 - t1 <= L And L <= l1 Then
            H = H + f(L - (l1 - t1), r1)
        End If
    End With
End Function

Public Function zh(L As Double) As Double
    zh = H(L, "ZH")
End Function

Public Function yh(L As Double) As Double
    yh = H(L, "YH")
End Function
