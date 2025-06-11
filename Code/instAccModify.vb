Public Class instAccModify
    Inherits instruction

    Public requireFlagC As Boolean = False
    Public operand As memoryTarget
    Public Sub New()
        type = InstructionType.ACC_MODIFY
    End Sub

    Public Overrides Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = requireFlagC
        z = False
        i = False
        d = False
        v = False
        n = False
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(operand)
        Dim t As memoryTarget = createCPURegisterMemoryTarget(CpuRegister.a)
        l.Add(t)
        Return l
    End Function

    Public Overrides Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(operand)
        Select Case operand.addrMode
            Case AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.ZERO_PAGE_INDEXED_X
                l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
            Case AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDIRECT_INDEXED_Y, AddressingMode.ZERO_PAGE_INDEXED_Y
                l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        End Select
        Dim t As memoryTarget = createCPURegisterMemoryTarget(CpuRegister.a)
        l.Add(t)
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return codeBlock.saveMemoryTargetToString(operand) & ";" & requireFlagC.ToString
    End Function

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        operand = codeBlock.loadMemoryTargetFromString(s(0))
        requireFlagC = CBool(s(1))
    End Sub

    Public Overrides Function printToCode(tabStr As String) As String
        Return printLabel() & tabStr & "op" & opName & "(" & printMemoryTargetToCode(operand, True) & ");" & vbCrLf
    End Function
End Class
