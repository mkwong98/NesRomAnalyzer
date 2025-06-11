Imports System.IO

Public Enum BlockType
    RESET
    NMI
    BRK
    SUBROUNTINE
    IF_THEN_ELSE
    IF_BLOCK
    Else_BLOCK
    BRANCH_LOOP
    INFINITE_LOOP
    LOOP_CONTENT
End Enum


Public Class block
    Inherits codeBlock

    Public name As String = ""
    Public requiredMem As New List(Of memoryTarget)
    Public overwrittenMem As New List(Of memoryTarget)
    Public flgCarryRequired As Boolean = False
    Public flgZeroRequired As Boolean = False
    Public flgIntrDisRequired As Boolean = False
    Public flgDecimalRequired As Boolean = False
    Public flgOverflowRequired As Boolean = False
    Public flgNegativeRequired As Boolean = False
    Public flgCarryOverwritten As Boolean = False
    Public flgZeroOverwritten As Boolean = False
    Public flgIntrDisOverwritten As Boolean = False
    Public flgDecimalOverwritten As Boolean = False
    Public flgOverflowOverwritten As Boolean = False
    Public flgNegativeOverwritten As Boolean = False

    Public type As BlockType
    Public code As New List(Of codeBlock)

    Public Overrides Function isBlock() As Boolean
        Return True
    End Function

    Public Overrides Sub getOverwrittemFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = flgCarryOverwritten
        z = flgZeroOverwritten
        i = flgIntrDisOverwritten
        d = flgDecimalOverwritten
        v = flgOverflowOverwritten
        n = flgNegativeOverwritten
    End Sub

    Public Overrides Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
        c = flgCarryRequired
        z = flgZeroRequired
        i = flgIntrDisRequired
        d = flgDecimalRequired
        v = flgOverflowRequired
        n = flgNegativeRequired
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Return requiredMem
    End Function

    Public Overrides Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
        Return overwrittenMem
    End Function

    Public Sub addCodeBlock(b As codeBlock)
        code.Add(b)
        Dim c, z, i, d, v, n As Boolean
        b.getRequiredFlags(c, z, i, d, v, n)
        flgCarryRequired = flgCarryRequired Or (c And Not flgCarryOverwritten)
        flgZeroRequired = flgZeroRequired Or (z And Not flgZeroOverwritten)
        flgIntrDisRequired = flgIntrDisRequired Or (i And Not flgIntrDisOverwritten)
        flgDecimalRequired = flgDecimalRequired Or (d And Not flgDecimalOverwritten)
        flgOverflowRequired = flgOverflowRequired Or (v And Not flgOverflowOverwritten)
        flgNegativeRequired = flgNegativeRequired Or (n And Not flgNegativeOverwritten)
        b.getOverwrittemFlags(c, z, i, d, v, n)
        flgCarryOverwritten = flgCarryOverwritten Or c
        flgZeroOverwritten = flgZeroOverwritten Or z
        flgIntrDisOverwritten = flgIntrDisOverwritten Or i
        flgDecimalOverwritten = flgDecimalOverwritten Or d
        flgOverflowOverwritten = flgOverflowOverwritten Or v
        flgNegativeOverwritten = flgNegativeOverwritten Or n
        Dim t As memoryTarget
        Dim m As memoryTarget
        Dim l As List(Of memoryTarget)
        l = b.getRequiredMemoryTarget
        For Each t In l
            Dim found As Boolean = False
            For Each m In overwrittenMem
                If t.addrMode = m.addrMode And t.realAddress.Type = m.realAddress.Type And t.realAddress.ID = m.realAddress.ID Then
                    found = True
                End If
            Next
            For Each m In requiredMem
                If t.addrMode = m.addrMode And t.realAddress.Type = m.realAddress.Type And t.realAddress.ID = m.realAddress.ID Then
                    found = True
                End If
            Next
            If Not found Then
                requiredMem.Add(t)
            End If
        Next
        l = b.getOverwrittenMemoryTarget
        For Each t In l
            Dim found As Boolean = False
            For Each m In overwrittenMem
                If t.addrMode = m.addrMode And t.realAddress.Type = m.realAddress.Type And t.realAddress.ID = m.realAddress.ID Then
                    found = True
                End If
            Next
            If Not found Then
                overwrittenMem.Add(t)
            End If
        Next
    End Sub

    Public Overrides Sub loadFromString(ByRef r As String)
        Dim i As Integer = r.IndexOf(vbCrLf)
        Dim s As String = r.Substring(0, i)
        r = r.Substring(i + Len(vbCrLf))

        'read header line
        Dim s1() As String = s.Split(","c)
        name = s1(1).Trim
        Select Case s1(2).Trim.Substring("Type=".Length).Trim
            Case "SUBROUNTINE"
                type = BlockType.SUBROUNTINE
            Case "RESET"
                type = BlockType.RESET
            Case "NMI"
                type = BlockType.NMI
            Case "BRK"
                type = BlockType.BRK
            Case "IF_THEN_ELSE"
                type = BlockType.IF_THEN_ELSE
            Case "IF_BLOCK"
                type = BlockType.IF_BLOCK
            Case "ELSE_BLOCK"
                type = BlockType.Else_BLOCK
            Case "BRANCH_LOOP"
                type = BlockType.BRANCH_LOOP
            Case "INFINITE_LOOP"
                type = BlockType.INFINITE_LOOP
            Case "LOOP_CONTENT"
                type = BlockType.LOOP_CONTENT
        End Select
        realAddress = Convert.ToInt32(s1(3).Trim.Substring("Entry=".Length).Trim, 16)

        'read code lines
        Do Until r.StartsWith("END BLOCK")
            Dim c As codeBlock = codeBlock.createCodeBlockFromString(r)
            code.Add(c)
        Loop
        'consume the "END BLOCK" line
        i = r.IndexOf(vbCrLf)
        If i = -1 Then
            r = ""
        Else
            r = r.Substring(i + Len(vbCrLf))
        End If

    End Sub

    Public Overrides Function saveToString() As String
        Dim r As String = "BLOCK," & vbTab & name & ", " & "Type="
        Select Case type
            Case BlockType.RESET
                r &= "RESET"
            Case BlockType.NMI
                r &= "NMI"
            Case BlockType.BRK
                r &= "BRK"
            Case BlockType.IF_THEN_ELSE
                r &= "IF_THEN_ELSE"
            Case BlockType.IF_BLOCK
                r &= "IF_BLOCK"
            Case BlockType.Else_BLOCK
                r &= "ELSE_BLOCK"
            Case BlockType.BRANCH_LOOP
                r &= "BRANCH_LOOP"
            Case BlockType.INFINITE_LOOP
                r &= "INFINITE_LOOP"
            Case BlockType.SUBROUNTINE
                r &= "SUBROUNTINE"
            Case BlockType.LOOP_CONTENT
                r &= "LOOP_CONTENT"
        End Select
        r &= ", Entry=" & realAddressToHexStr(realAddress) & vbCrLf

        For Each c As codeBlock In code
            r &= c.saveToString
        Next
        r &= "END BLOCK" & vbCrLf
        Return r
    End Function

    Public Sub takeAllInstructions(b As block)
        While b.code.Count > 0
            Dim c As codeBlock = b.code(0)
            If c.isBlock Then
                takeAllInstructions(c)
            Else
                code.Add(c)
            End If
            b.code.RemoveAt(0)
        End While
    End Sub

    Public Function codeRange() As addressRange
        Dim r As addressRange
        If code.Count > 0 Then
            If code(0).isBlock Then
                Dim b As block = code(0)
                r.rangeStart = b.codeRange.rangeStart
            Else
                Dim i As instruction = code(0)
                r.rangeStart = i.realAddress
            End If
            If code(code.Count - 1).isBlock Then
                Dim b As block = code(code.Count - 1)
                r.rangeEnd = b.codeRange.rangeEnd
            Else
                Dim i As instruction = code(code.Count - 1)
                r.rangeEnd = i.realAddress
            End If
        Else
            r.rangeStart = UInt32.MaxValue
            r.rangeEnd = UInt32.MaxValue
        End If
        Return r
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = ""
        Select Case type
            Case BlockType.RESET
                s &= "void game::reset(){" & vbCrLf
                s &= printContentToCode(tabStr)
                s &= tabStr & "}" & vbCrLf & vbCrLf
            Case BlockType.NMI
                s &= "void game::nmi(){" & vbCrLf
                s &= printContentToCode(tabStr)
                s &= tabStr & "}" & vbCrLf & vbCrLf
            Case BlockType.BRK
                s &= "void game::brk(){" & vbCrLf
                s &= printContentToCode(tabStr)
                s &= tabStr & "}" & vbCrLf & vbCrLf
            Case BlockType.SUBROUNTINE
                s &= "void game::" & name & "(){" & vbCrLf
                s &= printContentToCode(tabStr)
                If s.EndsWith("    return;" & vbCrLf) Then
                    s = s.Remove(s.Length - Len("    return;" & vbCrLf), Len("    return;" & vbCrLf))
                End If
                s &= tabStr & "}" & vbCrLf & vbCrLf
            Case BlockType.INFINITE_LOOP
                s &= "void game::repeat(){" & vbCrLf
                s &= code(0).printToCode(tabStr)
                s &= tabStr & "}" & vbCrLf & vbCrLf

            Case BlockType.BRANCH_LOOP
                If code.Count = 1 Then
                    s &= tabStr & "while(true){" & vbCrLf
                    s &= code(0).printToCode(tabStr)
                    s &= tabStr & "}" & vbCrLf
                ElseIf code(0).isBlock Then
                    If CType(code(1), instruction).type = InstructionType.JUMP Then
                        s &= tabStr & "while(true){" & vbCrLf
                        s &= code(0).printToCode(tabStr)
                        s &= CType(code(1), instruction).printLabel()
                        s &= tabStr & "}" & vbCrLf
                    Else
                        s &= tabStr & "do{" & vbCrLf
                        s &= code(0).printToCode(tabStr)
                        s &= CType(code(1), instruction).printLabel()
                        s &= tabStr & "}" & vbCrLf & tabStr & "while(" & code(1).printToCode("") & ");" & vbCrLf
                    End If
                Else
                    s &= CType(code(0), instruction).printLabel()
                    s &= tabStr & "while(" & code(0).printToCode("") & "){" & vbCrLf
                    s &= code(1).printToCode(tabStr)
                    If code.Count > 2 Then
                        s &= CType(code(2), instruction).printLabel()
                    End If
                    s &= tabStr & "}" & vbCrLf
                End If

            Case BlockType.IF_THEN_ELSE
                s &= CType(code(0), instruction).printLabel()
                s &= tabStr & "if(" & code(0).printToCode("") & "){" & vbCrLf
                s &= code(1).printToCode(tabStr)
                s &= tabStr & "}" & vbCrLf
                If code.Count > 2 Then
                    s &= tabStr & "else{" & vbCrLf
                    s &= code(2).printToCode(tabStr)
                    s &= tabStr & "}" & vbCrLf
                End If

            Case BlockType.LOOP_CONTENT, BlockType.IF_BLOCK, BlockType.Else_BLOCK
                s &= printContentToCode(tabStr)
        End Select
        Return s
    End Function

    Public Function printContentToCode(tabStr As String) As String
        Dim s As String = ""
        For Each c As codeBlock In code
            s &= c.printToCode(tabStr & "    ")
        Next
        Return s
    End Function

    Public Function printToHeader() As String
        Select Case type
            Case BlockType.RESET
                Return "void reset();" & vbCrLf
            Case BlockType.NMI
                Return "void nmi();" & vbCrLf
            Case BlockType.BRK
                Return "void brk();" & vbCrLf
            Case BlockType.INFINITE_LOOP
                Return "void repeat();" & vbCrLf
            Case BlockType.SUBROUNTINE
                Return "void " & name & "();" & vbCrLf
        End Select
        Return ""
    End Function

    Public Function getNextAddress() As UInt32
        If code.Count = 0 Then Return UInt32.MaxValue
        If code(code.Count - 1).isBlock Then
            Dim tBlock As block = code(code.Count - 1)
            Return tBlock.getNextAddress
        Else
            Dim tInst As instruction = code(code.Count - 1)
            Return tInst.nextAddress
        End If

    End Function
End Class
