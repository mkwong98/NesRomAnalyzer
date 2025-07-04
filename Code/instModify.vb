﻿Public Class instModify
    Inherits instruction

    Public requireFlagC As Boolean = False
    Public operand As memoryTarget
    Public Sub New()
        type = InstructionType.MODIFY
    End Sub

    Public Overrides Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = requireFlagC
        z = False
        i = False
        d = False
        v = False
        n = False
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        operand = codeBlock.loadMemoryTargetFromString(s(0))
        requireFlagC = CBool(s(1))
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(operand)
        Select Case operand.addrMode
            Case AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.ZERO_PAGE_INDEXED_X
                l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
            Case AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDIRECT_INDEXED_Y, AddressingMode.ZERO_PAGE_INDEXED_Y
                l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        End Select
        Return l
    End Function

    Public Overrides Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(operand)
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return codeBlock.saveMemoryTargetToString(operand) & ";" & requireFlagC.ToString
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        '"INC", "DEC", "INX", "DEX", "INY", "DEY", "ASL", "LSR", "ROL", "ROR"
        Select Case opName
            Case "INC", "DEC"
                Return printLabel() & tabStr & "op" & opName & "(" & printMemoryTargetAsAddress(operand) & ", 1);" & vbCrLf
            Case "INX", "DEX", "INY", "DEY"
                Return printLabel() & tabStr & "op" & opName & "(1);" & vbCrLf
            Case Else
                If operand.addrMode = AddressingMode.ACCUMULATOR Or operand.addrMode = AddressingMode.IMPLICIT Then
                    Return printLabel() & tabStr & "op" & opName & "_A(1);" & vbCrLf
                Else
                    Return printLabel() & tabStr & "op" & opName & "_M(" & printMemoryTargetAsAddress(operand) & ", 1);" & vbCrLf
                End If
        End Select
    End Function
End Class
