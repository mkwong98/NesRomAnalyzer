Public Class instBreakLoop
    Inherits instruction

    Public Sub New()
        type = InstructionType.BREAK_LOOP
    End Sub

    Public Sub setUpdateFlag(f As FlagID)

    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)

    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return ""
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Return printLabel() & tabStr & "break;" & vbCrLf
    End Function
End Class
