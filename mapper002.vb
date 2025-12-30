Public Class mapper002
    Inherits mapper

    Private prgROMLarge As Boolean
    Private prgRAMSize As UInt32

    Public Overrides Function getActualAddress(address As UShort, config As String) As List(Of memoryID)
        Dim r As New List(Of memoryID)
        r.Add(New memoryID)
        Return r
    End Function

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt32)
        If type = MemoryType.PRG_ROM Then
            prgROMLarge = size > 16384
        ElseIf type = MemoryType.PRG_RAM Then
            prgRAMSize = size
        End If
    End Sub

End Class
