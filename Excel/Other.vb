Option Explicit

Function getup(cell As Range) As String
    '返回引用单元格往上第一个非空单元格
    Dim r As Long
    Dim c As Long
    r = cell.Row
    c = cell.Column
    Do Until r = 1 Or Application.Cells(r, c)
          r = r - 1
    Loop
    getup = Application.Cells(r, c)
End Function

Function fai(d As Long) As Double
    '返回直径为 d 的钢筋单位重量
    fai = pi * (d ^ 2) / 4 * 7.86 / 1000
End Function
