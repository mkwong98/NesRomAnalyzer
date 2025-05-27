Public Class instNOP
    Inherits instruction

    Public Sub New()
        type = InstructionType.NOP
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Return
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return "-"
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Return tabStr & "//NOP" & vbCrLf
    End Function
End Class
