Public Class traceSetting
    Public sourceAddress As UInt32 = UInt32.MaxValue
    Public startAddress As UInt32 = UInt32.MaxValue
    Public stack As List(Of stackEntry) = New List(Of stackEntry)
End Class
