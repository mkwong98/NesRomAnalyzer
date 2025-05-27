Public Class mapper001
    Inherits mapper

    Public Overrides Sub write(address As UShort, value As Byte)
    End Sub

    Public Overrides Function getActualAddress(address As UShort) As memoryID
        Dim realAddress As memoryID
        Return realAddress
    End Function

    Public Overrides Function getMapperConfig() As UInt64
        Return 0
    End Function

    Public Overrides Sub setMapperConfig(config As UInt64)

    End Sub

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt16)

    End Sub
End Class
