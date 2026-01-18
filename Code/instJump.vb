Public Class instJump
    Inherits instruction

    Public isIndirect As Boolean
    Public jumpToAddress As UInt16
    Public jumpToRealAddress As New List(Of UInt32)
    Public indirectJumpTargets As New List(Of UInt16)
    Public indirectJumpTargetConfigs As New List(Of String)
    Public indirectJumpRealTargets As New List(Of UInt32)

    Public Sub New()
        type = InstructionType.JUMP
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        jumpToRealAddress.Clear()
        jumpToRealAddress.AddRange(hexStrToRealAddressList(s(0)))
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
        l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
        l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        l.Add(createCPURegisterMemoryTarget(CpuRegister.a))
        Return l
    End Function

    Public Overrides Function saveInstructionContentToString() As String
        Return realAddressListToHexStr(jumpToRealAddress) & ";" & addressToHexStr(jumpToAddress) & ";" & isIndirect.ToString & ";" & getIndirectJumpTargetString() & ";"
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

    Public Sub readIndirectJumpTargetString(r As String)
        indirectJumpTargets.Clear()
        Dim t() As String = Split(r, ",")
        For Each a As String In t
            If a <> "" Then
                Dim c() As String = Split(a, "@")
                indirectJumpTargets.Add(Convert.ToUInt16(c(0), 16))
                If c.Length > 1 Then
                    indirectJumpTargetConfigs.Add(c(1))
                Else
                    indirectJumpTargetConfigs.Add("")
                End If
            End If
        Next
    End Sub

    Public Overrides Function printToCode(tabStr As String) As String
        Dim s As String = printLabel()
        s &= tabStr & "jump(myMapper->readCPU(0x" & addressToHexStr(jumpToAddress) & ") + (myMapper->readCPU((0x" & addressToHexStr(jumpToAddress) & " + 1) & 0x00ff) << 8));" & vbCrLf
        Return s
    End Function
End Class
