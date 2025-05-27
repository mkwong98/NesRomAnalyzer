Public Class instTransfer
    Inherits instruction

    Public source As memoryTarget
    Public destination As memoryTarget

    Public Sub New()
        type = InstructionType.TRANSFER
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        source = codeBlock.loadMemoryTargetFromString(s(0))
        destination = codeBlock.loadMemoryTargetFromString(s(1))
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(source)
        Select Case source.addrMode
            Case AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.ZERO_PAGE_INDEXED_X
                l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
            Case AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDIRECT_INDEXED_Y, AddressingMode.ZERO_PAGE_INDEXED_Y
                l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        End Select
        Return l
    End Function

    Public Overrides Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(destination)
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return codeBlock.saveMemoryTargetToString(source) & ";" & codeBlock.saveMemoryTargetToString(destination)
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Select Case destination.addrMode
            Case AddressingMode.ZERO_PAGE, AddressingMode.ABSOLUTE, AddressingMode.ZERO_PAGE_INDEXED_X, AddressingMode.ZERO_PAGE_INDEXED_Y,
                AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.INDIRECT_INDEXED_Y
                Return tabStr & printMemoryTargetToCode(destination, False) & printMemoryTargetToCode(source, True) & ");" & vbCrLf
            Case Else
                Return tabStr & printMemoryTargetToCode(destination, False) & " = " & printMemoryTargetToCode(source, True) & ";" & vbCrLf

        End Select
    End Function
End Class
