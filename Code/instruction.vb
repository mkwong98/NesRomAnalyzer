Imports System.Reflection.Emit

Public Enum InstructionType
    TRANSFER 'Access and transfer
    MODIFY 'INC, DEC, INX, DEX, INY, DEY and shift 
    ACC_MODIFY 'ADC, SBC and Bitwise
    COMPARE
    BRANCH
    JUMP
    SUBROUTINE
    SUB_RETURN
    STACK
    FLAG
    NOP
    DIRECT_COMPARE
    COMPARE_BRANCH
    LOAD_BRANCH
    REPEATED_MODIFY
    JUMP_BLOCK
    BREAK_LOOP
End Enum

Public MustInherit Class instruction
    Inherits codeBlock

    Public type As InstructionType
    Public opName As String = ""
    Public flgCarryChange As Boolean = False
    Public flgZeroChange As Boolean = False
    Public flgIntrDisChange As Boolean = False
    Public flgDecimalChange As Boolean = False
    Public flgOverflowChange As Boolean = False
    Public flgNegativeChange As Boolean = False
    Public isJumpTarget As Boolean = False
    Public traceMarking As String = ""
    Public backSource As List(Of UInt32) = New List(Of UInt32) 'List of source address that use this instruction as a target
    Public subReturnAddresses As List(Of UInt32) = New List(Of UInt32) 'List of subroutine return addresses
    Public flgCReqAddress As UInt32 = UInt32.MaxValue
    Public flgZReqAddress As UInt32 = UInt32.MaxValue
    Public flgIReqAddress As UInt32 = UInt32.MaxValue
    Public flgDReqAddress As UInt32 = UInt32.MaxValue
    Public flgVReqAddress As UInt32 = UInt32.MaxValue
    Public flgNReqAddress As UInt32 = UInt32.MaxValue
    Public regAReqAddress As UInt32 = UInt32.MaxValue
    Public regXReqAddress As UInt32 = UInt32.MaxValue
    Public regYReqAddress As UInt32 = UInt32.MaxValue
    Public nextAddress As UInt32 = UInt32.MaxValue
    Public needLabel As Boolean = False

    Public Overrides Function isBlock() As Boolean
        Return False
    End Function

    Public Sub setFlagChange(c As Boolean, z As Boolean, i As Boolean, d As Boolean, v As Boolean, n As Boolean)
        flgCarryChange = c
        flgZeroChange = z
        flgIntrDisChange = i
        flgDecimalChange = d
        flgOverflowChange = v
        flgNegativeChange = n
    End Sub

    Public Overrides Sub getOverwrittemFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = flgCarryChange
        z = flgZeroChange
        i = flgIntrDisChange
        d = flgDecimalChange
        v = flgOverflowChange
        n = flgNegativeChange
    End Sub

    Public Overrides Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = False
        z = False
        i = False
        d = False
        v = False
        n = False
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Return New List(Of memoryTarget)
    End Function

    Public Overrides Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
        Return New List(Of memoryTarget)
    End Function

    Public Shared Function createInstructionFromString(ByRef r As String) As instruction
        Dim i As Integer = r.IndexOf(vbCrLf)
        Dim s As String
        If i = -1 Then
            s = r.Trim
            r = ""
        Else
            s = r.Substring(0, i).Trim
            r = r.Substring(i + 1).Trim
        End If
        Dim parts() As String = s.Split(","c)
        Dim type As String = parts(2).Trim
        Select Case type.Substring("Type=".Length).Trim
            Case "TRANSFER"
                Dim t As New instTransfer
                t.loadFromString(s)
                Return t
            Case "MODIFY"
                Dim t As New instModify
                t.loadFromString(s)
                Return t
            Case "ACC_MODIFY"
                Dim t As New instAccModify
                t.loadFromString(s)
                Return t
            Case "COMPARE"
                Dim t As New instCompare
                t.loadFromString(s)
                Return t
            Case "BRANCH"
                Dim t As New instBranch
                t.loadFromString(s)
                Return t
            Case "JUMP"
                Dim t As New instJump
                t.loadFromString(s)
                Return t
            Case "SUBROUTINE"
                Dim t As New instSubroutine
                t.loadFromString(s)
                Return t
            Case "SUB_RETURN"
                Dim t As New instSubReturn
                t.loadFromString(s)
                Return t
            Case "STACK"
                Dim t As New instStack
                t.loadFromString(s)
                Return t
            Case "FLAG"
                Dim t As New instFlag
                t.loadFromString(s)
                Return t
            Case "D_COMPARE"
                Dim t As New instDirectCompare
                t.loadFromString(s)
                Return t
            Case "C_BRANCH"
                Dim t As New instCompareBranch
                t.loadFromString(s)
                Return t
            Case "L_BRANCH"
                Dim t As New instLoadBranch
                t.loadFromString(s)
                Return t
            Case "R_MODIFY"
                Dim t As New instRepeatedModify
                t.loadFromString(s)
                Return t
            Case "JUMP_BLOCK"
                Dim t As New instJumpBlock
                t.loadFromString(s)
                Return t
            Case Else
                Dim t As New instNOP
                t.loadFromString(s)
                Return t
        End Select

    End Function

    Public Overrides Sub loadFromString(ByRef r As String)
        Dim s As String = r
        Dim parts() As String = s.Split(","c)
        realAddress = Convert.ToUInt32(parts(1).Trim, 16)
        opName = parts(3).Trim.Substring("OP=".Length).Trim

        Dim i As Integer = s.IndexOf("(")
        Dim j As Integer = s.IndexOf(")")
        Dim s1 As String = s.Substring(i + 1, j - i - 1).Trim
        loadInstructionContentFromString(s1)
        s = s.Substring(j + 1).Trim

        i = s.IndexOf("C=")
        flgCarryChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("Z=")
        flgZeroChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("I=")
        flgIntrDisChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("D=")
        flgDecimalChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("V=")
        flgOverflowChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("N=")
        flgNegativeChange = s.Substring(i + 2, 1) = "T"
        i = s.IndexOf("JUMP=")
        isJumpTarget = s.Substring(i + 5, 1) = "T"
    End Sub

    Public Overrides Function saveToString() As String
        Dim r As String = "INST," & vbTab & realAddressToHexStr(realAddress) & ", Type="
        Select Case type
            Case InstructionType.TRANSFER
                r &= "TRANSFER"
            Case InstructionType.MODIFY
                r &= "MODIFY"
            Case InstructionType.ACC_MODIFY
                r &= "ACC_MODIFY"
            Case InstructionType.COMPARE
                r &= "COMPARE"
            Case InstructionType.BRANCH
                r &= "BRANCH"
            Case InstructionType.JUMP
                r &= "JUMP"
            Case InstructionType.SUBROUTINE
                r &= "SUBROUTINE"
            Case InstructionType.SUB_RETURN
                r &= "SUB_RETURN"
            Case InstructionType.STACK
                r &= "STACK"
            Case InstructionType.FLAG
                r &= "FLAG"
            Case InstructionType.NOP
                r &= "NOP"
            Case InstructionType.DIRECT_COMPARE
                r &= "D_COMPARE"
            Case InstructionType.COMPARE_BRANCH
                r &= "C_BRANCH"
            Case InstructionType.LOAD_BRANCH
                r &= "L_BRANCH"
            Case InstructionType.REPEATED_MODIFY
                r &= "R_MODIFY"
            Case InstructionType.JUMP_BLOCK
                r &= "JUMP_BLOCK"
        End Select
        r &= ", OP=" & opName & ", "

        r &= "("
        r &= saveInstructionContentToString()
        r &= "), "

        r &= "C=" & flgCarryChange.ToString & ", "
        r &= "Z=" & flgZeroChange.ToString & ", "
        r &= "I=" & flgIntrDisChange.ToString & ", "
        r &= "D=" & flgDecimalChange.ToString & ", "
        r &= "V=" & flgOverflowChange.ToString & ", "
        r &= "N=" & flgNegativeChange.ToString & ", "
        r &= "JUMP=" & isJumpTarget.ToString & vbCrLf

        Return r
    End Function

    Public MustOverride Function saveInstructionContentToString() As String
    Public MustOverride Sub loadInstructionContentFromString(ByRef r As String)

    Public Function printTraceResult() As String
        Dim r As String = saveToString().Replace(vbCrLf, "")
        r &= ", " & traceMarking & printTraceDetail() & "," & vbTab
        For i As Integer = 0 To backSource.Count - 1
            If i > 0 Then
                r &= ";"
            End If
            r &= realAddressToHexStr(backSource(i))
        Next
        r &= vbCrLf
        Return r
    End Function

    Public Overridable Function printTraceDetail() As String
        Return ""
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = opName & vbCrLf
        Return s
    End Function

    Public Function printLabel() As String
        If Not needLabel Then
            Return ""
        End If
        Return "L_" & realAddressToHexStr(realAddress) & ":" & vbCrLf
    End Function
End Class
