Public Class instSubroutine
    Inherits instruction

    Public subRealAddress As UInt32
    Public subAddress As UInt16
    Public restoreFlags As Boolean

    Public hasReturned As Boolean = False ' Used to check if the subroutine has returned

    Public Sub New()
        type = InstructionType.SUBROUTINE
        hasReturned = False
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        subRealAddress = Convert.ToUInt32(s(0), 16)
        subAddress = Convert.ToUInt16(s(1), 16)
        restoreFlags = CBool(s(2))
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return subRealAddress.ToString("X6") & ";" & subAddress.ToString("X4") & ";" & restoreFlags.ToString
    End Function
End Class
