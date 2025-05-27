Public MustInherit Class mapper
    Public MustOverride Sub write(address As UInt16, value As Byte)

    Public MustOverride Function getActualAddress(address As UInt16) As memoryID

    Public MustOverride Sub setMemorySize(type As MemoryType, size As UInt16)

    Public MustOverride Function getMapperConfig() As UInt64

    Public MustOverride Sub setMapperConfig(config As UInt64)
End Class
