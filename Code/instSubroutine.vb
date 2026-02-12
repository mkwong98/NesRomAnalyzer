Public Class instSubroutine
    Inherits instruction

    Public subRealAddress As List(Of UInt32)
    Public subFixedRealAddress As List(Of UInt32)
    Public subAddress As UInt16
    Public restoreFlags As Boolean

    Public hasReturned As Boolean = False ' Used to check if the subroutine has returned

    Public Sub New()
        type = InstructionType.SUBROUTINE
        hasReturned = False
        subRealAddress = New List(Of UInt32)
        subFixedRealAddress = New List(Of UInt32)
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        readRealJumpTargetString(s(0))
        If s.Length = 3 Then
            subAddress = Convert.ToUInt16(s(1), 16)
            restoreFlags = CBool(s(2))
        Else
            readFixedRealJumpTargetString(s(1))
            subAddress = Convert.ToUInt16(s(2), 16)
            restoreFlags = CBool(s(3))
        End If

    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Return getRealJumpTargetString() & ";" & getFixedRealJumpTargetString() & ";" & addressToHexStr(subAddress) & ";" & restoreFlags.ToString
    End Function

    Public Overrides Function getRequiredMemoryTarget() As List(Of memoryTarget)
        Dim l As New List(Of memoryTarget)
        l.Add(createCPURegisterMemoryTarget(CpuRegister.x))
        l.Add(createCPURegisterMemoryTarget(CpuRegister.y))
        l.Add(createCPURegisterMemoryTarget(CpuRegister.a))
        Return l
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

    Public Sub readFixedRealJumpTargetString(r As String)
        subFixedRealAddress.Clear()
        Dim t() As String = Split(r, ",")
        For Each a As String In t
            If a <> "" Then
                subFixedRealAddress.Add(Convert.ToUInt32(a, 16))
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

    Public Function getFixedRealJumpTargetString() As String
        Dim r As String = ""
        For Each a As UInt32 In subFixedRealAddress
            If r <> "" Then
                r = r & ","
            End If
            r = r & realAddressToHexStr(a)
        Next
        Return r
    End Function
End Class
