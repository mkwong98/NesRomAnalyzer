Public Class traceTask
    Public type As TaskType
    Public realAddress As UInt32
    Public mapperConfig As UInt64
    Public name As String
    Public endWithReturn As Boolean = False
    Public endWithSkip As Boolean = False
    Public source As UInt32 'Address of the instruction that called this task
    Public rtsAddress As New List(Of UInt32)
    Public stack As List(Of stackEntry) = New List(Of stackEntry)

End Class
