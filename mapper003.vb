Public Class mapper003
    Inherits mapper

    Private prgROMLarge As Boolean
    Private prgRAMSize As UInt32

    Public Overrides Function getActualAddress(address As UShort, config As bankConfig) As List(Of memoryID)
        Dim result As New List(Of memoryID)
        Dim realAddress As memoryID
        realAddress.config.inUse = False
        If address >= &H8000 Then
            realAddress.Type = MemoryType.PRG_ROM
            If prgROMLarge Then
                realAddress.ID = address And &H7FFF
            Else
                realAddress.ID = address And &H3FFF
            End If
        ElseIf (address >= &H6000) And prgRAMSize > 0 Then
            realAddress.Type = MemoryType.PRG_RAM
            realAddress.ID = address Mod prgRAMSize
        Else
            realAddress.Type = MemoryType.DISABLED
        End If
        result.Add(realAddress)
        Return result
    End Function

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt32)
        If type = MemoryType.PRG_ROM Then
            prgROMLarge = size > 16384
        ElseIf type = MemoryType.PRG_RAM Then
            prgRAMSize = size
        End If
    End Sub

End Class
