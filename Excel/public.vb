Option Explicit

Public Const pi As Double = 3.14159265358979

Public Function find_l_in_file(L As Double, file As String) As Long
    '被 H 和 XY 函数调用
    '也可单独调用
    Dim r As Long
    r = 2
    With Worksheets(file)
        Do Until .Cells(r, 1) <= L And L <= .Cells(r + 1, 1) Or IsEmpty(.Cells(r + 1, 1)) 'IsEmpty 很重要
            r = r + 1
        Loop
        If IsEmpty(.Cells(r + 1, 1)) Then
            MsgBox "Error: L out of range!" & vbCrLf & "L=" & L _
                    & vbCrLf & "row=" & r _
                    & vbCrLf & "L0=" & .Cells(r, 1) _
                    & vbCrLf & "L1=" & .Cells(r + 1, 1)
            find_l_in_file = False
            Exit Function
        Else
            find_l_in_file = r
        End If
    End With
End Function
