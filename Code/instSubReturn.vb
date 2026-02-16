Public Enum SubReturnType
    NORMAL
    INDIRECT_JUMP
    SKIP_TO_PREVIOUS
    UNKNOWN
End Enum

Public Class instSubReturn
    Inherits instruction

    Public restoreFlags As Boolean
    Public returnType As SubReturnType = SubReturnType.UNKNOWN
    Public Sub New()
        type = InstructionType.SUB_RETURN
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        restoreFlags = CBool(r)
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return restoreFlags.ToString
    End Function

    Public Overrides Function printTraceDetail() As String
        Select Case returnType
            Case SubReturnType.NORMAL
                Return ", Normal return"
            Case SubReturnType.INDIRECT_JUMP
                Return ", Indirect jump"
            Case SubReturnType.SKIP_TO_PREVIOUS
                Return ", Skip to previous"
            Case SubReturnType.UNKNOWN
                Return ", Unknown return"
        End Select
        Return ""
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printLabel()
        If Not restoreFlags Then
            s &= tabStr & "popAddress();" & vbCrLf
        Else
            s &= tabStr & "popStatus();" & vbCrLf
            s &= tabStr & "popIRAddress();" & vbCrLf
        End If
        s &= tabStr & "return;" & vbCrLf
        Return s
    End Function
End Class
