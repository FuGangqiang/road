Option Explicit

Private Function i(l As Double, file As String) As Double
    '���������߱仯
    '����·������
    'ע�ⷵ��ֵ�������ŵ�����
    Dim l0 As Double, l1 As Double
    Dim i0 As Double, i1 As Double
    Dim dl As Double, lc As Double
    Dim r As Long
    r = find_l_in_file(l, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        i0 = .Cells(r, 2)
        l1 = .Cells(r + 1, 1)
        i1 = .Cells(r + 1, 2)
        lc = l1 - l0
        If i0 = i1 Then '�޳���
            i = i0
        Else '�г���
            dl = l - l0
            i = i0 + (i1 - i0) * (dl ^ 2 / lc ^ 2) * (3 - 2 * dl / lc)
        End If
    End With
End Function

Private Function ii(l As Double, file As String) As Double
    '���Ա仯
    '����·������
    'ע�ⷵ��ֵ�������ŵ�����
    Dim l0 As Double, l1 As Double
    Dim i0 As Double, i1 As Double
    Dim dl As Double, lc As Double
    Dim r As Long
    r = find_l_in_file(l, file)
    With Worksheets(file)
        l0 = .Cells(r, 1)
        i0 = .Cells(r, 2)
        l1 = .Cells(r + 1, 1)
        i1 = .Cells(r + 1, 2)
        lc = l1 - l0
        If i0 = i1 Then '�޳���
            ii = i0
        Else '�г���
            dl = l - l0
            ii = i0 + (i1 - i0) * dl / lc
        End If
    End With
End Function

Public Function yi(l As Double) As Double
    yi = ii(l, "YHCG")
End Function


Public Function zi(l As Double) As Double
    zi = ii(l, "ZHCG")
End Function
