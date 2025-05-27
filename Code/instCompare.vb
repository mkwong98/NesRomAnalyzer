Public Class instCompare
    Inherits instruction

    Public operand1 As CpuRegister
    Public operand2 As memoryTarget

    Public Sub New()
        flgCarryChange = True
        flgZeroChange = True
        flgNegativeChange = True
        type = InstructionType.COMPARE
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        Select Case s(0)
            Case "A"
                operand1 = CpuRegister.a
            Case "X"
                operand1 = CpuRegister.x
            Case "Y"
                operand1 = CpuRegister.y
        End Select
        operand2 = codeBlock.loadMemoryTargetFromString(s(1))
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(codeBlock.createCPURegisterMemoryTarget(operand1))
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
        Dim s As String = ""
        Select Case operand1
            Case CpuRegister.a
                s &= "A"
            Case CpuRegister.x
                s &= "X"
            Case CpuRegister.y
                s &= "Y"
        End Select
        Return s & ";" & codeBlock.saveMemoryTargetToString(operand2)
    End Function
End Class
