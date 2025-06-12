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
                Return printLabel() & tabStr & printMemoryTargetToCode(destination, False) & printMemoryTargetToCode(source, True) & ");" & vbCrLf
            Case Else
                Dim s As String = printLabel()
                s &= tabStr & printMemoryTargetToCode(destination, False) & " = " & printMemoryTargetToCode(source, True) & ";" & vbCrLf
                If destination.addrMode = AddressingMode.ACCUMULATOR _
                    Or (destination.addrMode = AddressingMode.IMPLICIT And (destination.realAddress.ID = CpuRegister.a Or destination.realAddress.ID = CpuRegister.x Or destination.realAddress.ID = CpuRegister.y)) Then
                    s &= tabStr & "setLoadFlag(" & printMemoryTargetToCode(destination, False) & ");" & vbCrLf
                End If
                Return s
        End Select
    End Function
End Class
