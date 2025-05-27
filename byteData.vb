Public Class byteData
    Public r As addressRange
    Public isRunLength As Boolean = False
    Public runLength As Integer = 0
    Public runLengthByte As Byte = 0
    Public data As New List(Of Byte)

    Public Shared Function generateByteData(pData() As Byte, pStart As Integer, pEnd As Integer) As List(Of byteData)
        Dim result As New List(Of byteData)
        Dim i As Integer = pStart
        Dim b As byteData
        If pStart = pEnd Then
            b = New byteData
            b.r.rangeStart = pStart
            b.r.rangeEnd = pEnd
            b.data.Add(pData(pStart))
            result.Add(b)
            Return result
        End If
        b = New byteData
        Do Until i > pEnd
            'check run length
            Dim runLength As Integer = 1
            Dim runLengthByte As Byte = pData(i)
            Dim runEnd As Boolean = False
            Do Until runEnd
                If i + runLength > pEnd Then
                    runEnd = True
                ElseIf pData(i + runLength) = runLengthByte Then
                    runLength += 1
                    If i + runLength > pEnd Then
                        runEnd = True
                    End If
                Else
                    runEnd = True
                End If
            Loop
            If runLength < 15 Then
                b.data.Add(pData(i))
                b.r.rangeEnd = i
                i += 1
            Else
                If b.data.Count > 0 Then
                    result.Add(b)
                End If
                b = New byteData
                b.isRunLength = True
                b.runLength = runLength
                b.runLengthByte = runLengthByte
                b.r.rangeStart = i
                b.r.rangeEnd = i + runLength - 1
                result.Add(b)
                i += runLength
                b = New byteData
                b.r.rangeStart = i
                b.r.rangeEnd = i
            End If
        Loop
        If b.data.Count > 0 Then
            result.Add(b)
        End If
        Return result
    End Function

    Public Sub setRunLength(runLength As Integer, runLengthByte As Byte)
        isRunLength = True
        runLength = runLength
        runLengthByte = runLengthByte
    End Sub

    Public Sub setData(pData As List(Of Byte))
        isRunLength = False
        runLength = 0
        runLengthByte = 0
        data = New List(Of Byte)
        data.AddRange(pData)
    End Sub
End Class
