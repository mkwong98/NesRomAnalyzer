Public Class instFlag
    Inherits instruction

    Public updateFlag As FlagID
    Public isClear As Boolean

    Public Sub New()
        type = InstructionType.FLAG
    End Sub

    Public Sub setUpdateFlag(f As FlagID)
        updateFlag = f
        flgCarryChange = False
        flgZeroChange = False
        flgIntrDisChange = False
        flgDecimalChange = False
        flgOverflowChange = False
        flgNegativeChange = False
        Select Case updateFlag
            Case FlagID.c
                flgCarryChange = True
            Case FlagID.d
                flgZeroChange = True
            Case FlagID.i
                flgIntrDisChange = True
            Case FlagID.n
                flgDecimalChange = True
            Case FlagID.v
                flgOverflowChange = True
            Case FlagID.z
                flgNegativeChange = True
        End Select
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        Select Case s(0).Trim.ToUpper
            Case "C"
                updateFlag = FlagID.c
            Case "D"
                updateFlag = FlagID.d
            Case "I"
                updateFlag = FlagID.i
            Case "N"
                updateFlag = FlagID.n
            Case "V"
                updateFlag = FlagID.v
            Case "Z"
                updateFlag = FlagID.z
        End Select
        isClear = CBool(s(1))
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Dim s As String = ""
        Select Case updateFlag
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
        s &= ";" & isClear.ToString
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = tabStr

        Select Case updateFlag
            Case FlagID.c
                s &= "flgC"
            Case FlagID.i
                s &= "flgI"
            Case FlagID.d
                s &= "flgD"
            Case FlagID.z
                s &= "flgV"
        End Select
        If isClear Then
            s &= " = false"
        Else
            s &= " = true"
        End If
        s &= ";" & vbCrLf
        Return s
    End Function
End Class
