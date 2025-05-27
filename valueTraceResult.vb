Public Class valueTraceResult
    Public values As New List(Of addressRange)
    Public valueValidAt As UInt32
    Public valueFor As memoryID

    Public Sub addRange(r As addressRange)
        Dim found As Boolean = False
        Dim tmp As addressRange
        For i As Integer = 0 To values.Count - 1
            If values(i).rangeStart <= r.rangeEnd + 1 And values(i).rangeEnd + 1 >= r.rangeStart Then
                found = True
                tmp = values(i)
                If tmp.rangeStart > r.rangeStart Then
                    tmp.rangeStart = r.rangeStart
                End If
                If tmp.rangeEnd < r.rangeEnd Then
                    tmp.rangeEnd = r.rangeEnd
                End If
                values(i) = tmp
            End If
        Next
        If Not found Then
            values.Add(r)
        End If
        'merge overlapping ranges
        For i As Integer = 0 To values.Count - 1
            For j As Integer = values.Count - 1 To i + 1 Step -1
                If values(i).rangeStart <= values(j).rangeEnd + 1 And values(i).rangeEnd + 1 >= values(j).rangeStart Then
                    tmp = values(i)
                    If tmp.rangeStart > values(j).rangeStart Then
                        tmp.rangeStart = values(j).rangeStart
                    End If
                    If tmp.rangeEnd < values(j).rangeEnd Then
                        tmp.rangeEnd = values(j).rangeEnd
                    End If
                    values(i) = tmp
                End If
            Next
        Next
    End Sub
End Class
