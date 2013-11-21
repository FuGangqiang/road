Option Explicit

Private Function i(L As Double, file As String) As Double
    '计算路拱横坡
    '注意返回值得正负号的意义
    Dim l0 As Double, l1 As Double
    Dim i0 As Double, i1 As Double
    Dim dl As Double, lc As Double
    Dim r As Long
    r = find_l_in_file(L, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        i0 = .Cells(r, 2)
        l1 = .Cells(r + 1, 1)
        i1 = .Cells(r + 1, 2)
        lc = l1 - l0
        If i0 = i1 Then '无超高
            i = i0
        Else '有超高
            dl = L - l0
            i = i0 + (i1 - i0) * (dl ^ 2 / lc ^ 2) * (3 - 2 * dl / lc)
        End If
    End With
End Function

Public Function zi(L As Double) As Double
    zi = i(L, "ZHCG")
End Function

Public Function yi(L As Double) As Double
    yi = i(L, "YHCG")
End Function

