Public Enum JumpBlockType
    JSR
    JMP
    BRK
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
        End Select
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = ""
        Select Case jumpType
            Case JumpBlockType.JSR
                'push pc to stack
                s &= tabStr & "mStack.push({true, " & realAddressToHexStr(realAddress) & "});" & vbCrLf
                s &= tabStr & "mStack.push({true, " & realAddressToHexStr(realAddress) & "});" & vbCrLf
                s &= tabStr & blockName & "();" & vbCrLf
                s &= tabStr & "poppedEntry = mStack.top();" & vbCrLf
                s &= tabStr & "mStack.pop();" & vbCrLf
                s &= tabStr & "mStack.pop();" & vbCrLf
                s &= tabStr & "if (poppedEntry.value != " & realAddressToHexStr(realAddress) & ") return;" & vbCrLf

            Case JumpBlockType.JMP
                s &= tabStr & blockName & "();" & vbCrLf
                s &= tabStr & "return;" & vbCrLf

            Case JumpBlockType.BRK
                s &= tabStr & "mStack.push({true, " & realAddressToHexStr(realAddress) & "});" & vbCrLf
                s &= tabStr & "mStack.push({true, " & realAddressToHexStr(realAddress) & "});" & vbCrLf
                s &= tabStr & "flgB = true;" & vbCrLf
                s &= tabStr & "mStack.push({false, getStatus()});" & vbCrLf
                s &= tabStr & blockName & "();" & vbCrLf
                s &= tabStr & "poppedEntry = mStack.top();" & vbCrLf
                s &= tabStr & "setStatus(poppedEntry.value);" & vbCrLf
                s &= tabStr & "flgB = false;" & vbCrLf
                s &= tabStr & "mStack.pop();" & vbCrLf
                s &= tabStr & "mStack.pop();" & vbCrLf
                s &= tabStr & "mStack.pop();" & vbCrLf

        End Select
        Return s
    End Function
End Class
