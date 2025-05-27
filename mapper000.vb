﻿Public Class mapper000
    Inherits mapper

    Private prgROMLarge As Boolean
    Private prgRAMSize As UInt32

    Public Overrides Sub write(address As UShort, value As Byte)
    End Sub

    Public Overrides Function getActualAddress(address As UShort) As memoryID
        Dim realAddress As memoryID
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

        Return realAddress
    End Function

    Public Overrides Function getMapperConfig() As UInt64
        Return 0
    End Function

    Public Overrides Sub setMapperConfig(config As UInt64)

    End Sub

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt16)
        If type = MemoryType.PRG_ROM Then
            prgROMLarge = size > 16384
        ElseIf type = MemoryType.PRG_RAM Then
            prgRAMSize = size
        End If
    End Sub

End Class
