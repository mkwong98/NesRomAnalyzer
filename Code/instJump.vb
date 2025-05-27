Public Class instJump
    Inherits instruction

    Public isIndirect As Boolean
    Public jumpToRealAddress As UInt32
    Public jumpToAddress As UInt16

    Public Sub New()
        type = InstructionType.JUMP
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        jumpToRealAddress = Convert.ToUInt32(s(0), 16)
        jumpToAddress = Convert.ToUInt16(s(1), 16)
        isIndirect = CBool(s(2))
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        If isIndirect Then
            Dim t As memoryTarget
            t.addrMode = AddressingMode.INDIRECT
            t.address = jumpToAddress
            t.realAddress.Type = MemoryType.PRG_ROM
            t.realAddress.ID = jumpToRealAddress
            l.Add(t)
        End If
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return jumpToRealAddress.ToString("X6") & ";" & jumpToAddress.ToString("X4") & ";" & isIndirect.ToString
    End Function
End Class
