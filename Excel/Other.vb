Option Explicit

Function getup(cell As Range) As String
    '�������õ�Ԫ�����ϵ�һ���ǿյ�Ԫ��
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
    '����ֱ��Ϊ d �ĸֽλ����
    fai = pi * (d ^ 2) / 4 * 7.86 / 1000
End Function
