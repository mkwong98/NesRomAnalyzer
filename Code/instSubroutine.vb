Public Class instSubroutine
    Inherits instruction

    Public subRealAddress As List(Of UInt32)
    Public subAddress As UInt16
    Public restoreFlags As Boolean

    Public hasReturned As Boolean = False ' Used to check if the subroutine has returned

    Public Sub New()
        type = InstructionType.SUBROUTINE
        hasReturned = False
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        readRealJumpTargetString(s(0))
        subAddress = Convert.ToUInt16(s(1), 16)
        restoreFlags = CBool(s(2))
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return getRealJumpTargetString() & ";" & addressToHexStr(subAddress) & ";" & restoreFlags.ToString
    End Function

    Public Sub readRealJumpTargetString(r As String)
        subRealAddress.Clear()
        Dim t() As String = Split(r, ",")
        For Each a As String In t
            If a <> "" Then
                subRealAddress.Add(Convert.ToUInt32(a, 16))
            End If
        Next
    End Sub

    Public Function getRealJumpTargetString() As String
        Dim r As String = ""
        For Each a As UInt32 In subRealAddress
            If r <> "" Then
                r = r & ","
            End If
            r = r & realAddressToHexStr(a)
        Next
        Return r
    End Function
End Class
