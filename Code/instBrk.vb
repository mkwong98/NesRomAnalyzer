Public Class instBrk
    Inherits instruction

    Public Sub New()
        type = InstructionType.BRK
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Throw New NotImplementedException()
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return "BRK"
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printLabel() & tabStr & "brk();" & vbCrLf
        Return s
    End Function
End Class
