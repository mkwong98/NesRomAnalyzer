Public Class instLoadBranch
    Inherits instPBranch

    Public operand As memoryTarget

    Public Sub New()
        flgZeroChange = True
        flgNegativeChange = True
        type = InstructionType.LOAD_BRANCH
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        operand = codeBlock.loadMemoryTargetFromString(s(0))
        Select Case s(2).Trim.ToUpper
            Case "C"
                useFlag = FlagID.c
            Case "D"
                useFlag = FlagID.d
            Case "I"
                useFlag = FlagID.i
            Case "N"
                useFlag = FlagID.n
            Case "V"
                useFlag = FlagID.v
            Case "Z"
                useFlag = FlagID.z
        End Select
        flagIsSet = CBool(s(3))
        branchToAddress = Convert.ToUInt32(s(4), 16)
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

    Public Overrides Function saveInstructionContentToString() As String
        Dim s As String = codeBlock.saveMemoryTargetToString(operand) & ";"
        Select Case useFlag
            Case FlagID.c
                s &= "C"
            Case FlagID.d
                s &= "D"
            Case FlagID.i
                s &= "I"
            Case FlagID.n
                s &= "N"
            Case FlagID.v
                s &= "V"
            Case FlagID.z
                s &= "Z"
        End Select
        s &= ";" & flagIsSet.ToString & ";" & realAddressToHexStr(branchToAddress)
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printMemoryTargetToCode(operand, True)
        If flagIsSet Then
            Select Case useFlag
                Case FlagID.z
                    s &= " == 0"
                Case FlagID.n
                    s &= " & 0x80"
            End Select
        Else
            Select Case useFlag
                Case FlagID.z
                    s &= " != 0"
                Case FlagID.n
                    s = "!(" & s & " & 0x80)"
            End Select
        End If

        Return s
    End Function
End Class
