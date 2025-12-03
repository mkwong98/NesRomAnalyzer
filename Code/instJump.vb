Public Class instJump
    Inherits instruction

    Public isIndirect As Boolean
    Public jumpToRealAddress As New List(Of UInt32)
    Public jumpToAddress As UInt16
    Public indirectJumpTargets As New List(Of UInt16)
    Public indirectJumpRealTargets As New List(Of UInt32)

    Public Sub New()
        type = InstructionType.JUMP
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        readRealJumpTargetString(s(0))
        jumpToAddress = Convert.ToUInt16(s(1), 16)
        isIndirect = CBool(s(2))
        If s.Length > 3 Then
            readIndirectJumpTargetString(s(3))
        End If
    End Sub

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        If isIndirect Then
            For i As Integer = 0 To indirectJumpTargets.Count - 1
                Dim t As memoryTarget
                t.addrMode = AddressingMode.INDIRECT
                t.address = indirectJumpTargets(i)
                t.realAddress.Type = MemoryType.PRG_ROM
                t.realAddress.ID = indirectJumpRealTargets(i)
                l.Add(t)
            Next
        End If
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return getRealJumpTargetString() & ";" & addressToHexStr(jumpToAddress) & ";" & isIndirect.ToString & ";" & getIndirectJumpTargetString() & ";"
    End Function

    Public Function getIndirectJumpTargetString() As String
        Dim r As String = ""
        For Each a As UInt16 In indirectJumpTargets
            If r <> "" Then
                r = r & ","
            End If
            r = r & addressToHexStr(a)
        Next
        Return r
    End Function

    Public Function getRealJumpTargetString() As String
        Dim r As String = ""
        For Each a As UInt32 In jumpToRealAddress
            If r <> "" Then
                r = r & ","
            End If
            r = r & realAddressToHexStr(a)
        Next
        Return r
    End Function

    Public Sub readIndirectJumpTargetString(r As String)
        indirectJumpTargets.Clear()
        Dim t() As String = Split(r, ",")
        For Each a As String In t
            If a <> "" Then
                indirectJumpTargets.Add(Convert.ToUInt16(a, 16))
            End If
        Next
    End Sub

    Public Sub readRealJumpTargetString(r As String)
        jumpToRealAddress.Clear()
        Dim t() As String = Split(r, ",")
        For Each a As String In t
            If a <> "" Then
                jumpToRealAddress.Add(Convert.ToUInt32(a, 16))
            End If
        Next
    End Sub

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printLabel()
        s &= tabStr & "indirectJump(myMapper->readCPU(0x" & addressToHexStr(jumpToAddress) & ") + (myMapper->readCPU((0x" & addressToHexStr(jumpToAddress) & " + 1) & 0x00ff) << 8));" & vbCrLf
        Return s
    End Function
End Class
