Public Enum JumpBlockType
    JSR
    JMP
    BRK
    JGT
End Enum
Public Class instJumpBlock
    Inherits instruction

    Public blockName As String
    Public jumpType As JumpBlockType

    Public Sub New()
        type = InstructionType.JUMP_BLOCK
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        blockName = s(0)
        Select Case s(1)
            Case "JSR"
                jumpType = JumpBlockType.JSR
            Case "JMP"
                jumpType = JumpBlockType.JMP
            Case "BRK"
                jumpType = JumpBlockType.BRK
            Case "JGT"
                jumpType = JumpBlockType.JGT
        End Select
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Dim s As String = blockName & ";"
        Select Case jumpType
            Case JumpBlockType.JSR
                s &= "JSR"
            Case JumpBlockType.JMP
                s &= "JMP"
            Case JumpBlockType.BRK
                s &= "BRK"
            Case JumpBlockType.JGT
                s &= "JGT"
        End Select
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printLabel()
        Select Case jumpType
            Case JumpBlockType.JSR
                'push pc to stack
                s &= tabStr & "pushAddress(0x" & realAddressToHexStr(realAddress + 2) & ");" & vbCrLf
                s &= tabStr & blockName & vbCrLf
                s &= tabStr & "if (handleReturnAddress(poppedEntry.value, 0x" & realAddressToHexStr(realAddress + 2) & ")) return;" & vbCrLf

            Case JumpBlockType.JMP
                s &= tabStr & blockName & vbCrLf
                s &= tabStr & "return;" & vbCrLf

            Case JumpBlockType.BRK
                s &= tabStr & "pushAddress(0x" & realAddressToHexStr(realAddress + 2) & ");" & vbCrLf
                s &= tabStr & "flgB = true;" & vbCrLf
                s &= tabStr & "pushStatus();" & vbCrLf
                s &= tabStr & blockName & vbCrLf
                s &= tabStr & "popStatus();" & vbCrLf
                s &= tabStr & "flgB = false;" & vbCrLf
                s &= tabStr & "popAddress();" & vbCrLf

            Case JumpBlockType.JGT
                s &= tabStr & "goto " & blockName & ";" & vbCrLf
        End Select
        Return s
    End Function
End Class
