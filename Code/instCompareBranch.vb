Public Class instCompareBranch
    Inherits instPBranch

    Public operand1 As memoryTarget
    Public operand2 As memoryTarget

    Public Sub New()
        flgCarryChange = True
        flgZeroChange = True
        flgNegativeChange = True
        type = InstructionType.COMPARE_BRANCH
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        operand1 = codeBlock.loadMemoryTargetFromString(s(0))
        operand2 = codeBlock.loadMemoryTargetFromString(s(1))
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
        branchAddress = Convert.ToUInt16(s(4), 16)
        branchToAddress.Clear()
        branchToAddress.AddRange(hexStrToRealAddressList(s(5)))
        branchToFixedAddress.Clear()
        branchToFixedAddress.AddRange(hexStrToRealAddressList(s(6)))
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
        Dim s As String = codeBlock.saveMemoryTargetToString(operand1) & ";" & codeBlock.saveMemoryTargetToString(operand2) & ";"
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
        s &= ";" & flagIsSet.ToString & ";" & addressToHexStr(branchAddress) & ";" & realAddressListToHexStr(branchToAddress) & ";" & realAddressListToHexStr(branchToFixedAddress)
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printMemoryTargetToCode(operand1, True)
        If flagIsSet Then
            Select Case useFlag
                Case FlagID.c
                    s &= " >= "
                Case FlagID.z
                    s &= " == "
                Case FlagID.n
                    s &= " < "
            End Select
        Else
            Select Case useFlag
                Case FlagID.c
                    s &= " < "
                Case FlagID.z
                    s &= " != "
                Case FlagID.n
                    s &= " >= "
            End Select
        End If
        s &= printMemoryTargetToCode(operand2, True)
        Return s
    End Function
End Class
