Public Class instDirectCompare
    Inherits instruction

    Public operand1 As memoryTarget
    Public operand2 As memoryTarget

    Public Sub New()
        flgCarryChange = True
        flgZeroChange = True
        flgNegativeChange = True
        type = InstructionType.DIRECT_COMPARE
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        operand1 = codeBlock.loadMemoryTargetFromString(s(0))
        operand2 = codeBlock.loadMemoryTargetFromString(s(1))
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(operand1)
        Select Case operand1.addrMode
            Case AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.ZERO_PAGE_INDEXED_X
                l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
            Case AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDIRECT_INDEXED_Y, AddressingMode.ZERO_PAGE_INDEXED_Y
                l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        End Select
        l.Add(operand2)
        Select Case operand2.addrMode
            Case AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.ZERO_PAGE_INDEXED_X
                l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
            Case AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDIRECT_INDEXED_Y, AddressingMode.ZERO_PAGE_INDEXED_Y
                l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        End Select
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return codeBlock.saveMemoryTargetToString(operand1) & ";" & codeBlock.saveMemoryTargetToString(operand2)
    End Function
End Class
