Public Class instBranch
    Inherits instPBranch
    Public Sub New()
        type = InstructionType.BRANCH
    End Sub

    Public Overrides Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = False
        z = False
        i = False
        d = False
        v = False
        n = False
        Select Case useFlag
            Case FlagID.c
                c = True
            Case FlagID.d
                d = True
            Case FlagID.i
                i = True
            Case FlagID.n
                n = True
            Case FlagID.v
                v = True
            Case FlagID.z
                z = True
        End Select
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        Select Case s(0).Trim.ToUpper
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
        flagIsSet = CBool(s(1))
        branchAddress = Convert.ToUInt16(s(2), 16)
        branchToAddress.Clear()
        branchToAddress.AddRange(hexStrToRealAddressList(s(3)))
        branchToFixedAddress.Clear()
        branchToFixedAddress.AddRange(hexStrToRealAddressList(s(4)))
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Dim s As String = ""
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
        Dim s As String = ""
        If Not flagIsSet Then
            s &= "!"
        End If
        Select Case useFlag
            Case FlagID.c
                s &= "flgC"
            Case FlagID.z
                s &= "flgZ"
            Case FlagID.n
                s &= "flgN"
            Case FlagID.v
                s &= "flgV"
        End Select
        Return s
    End Function
End Class
