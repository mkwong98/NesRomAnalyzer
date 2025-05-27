Imports System.IO

Public Structure memoryTarget
    Public addrMode As AddressingMode
    Public address As UInt16
    Public realAddress As memoryID
End Structure

Public MustInherit Class codeBlock
    Public realAddress As UInt32

    Public Shared Function createCPURegisterMemoryTarget(r As CpuRegister) As memoryTarget
        Dim t As memoryTarget
        t.addrMode = AddressingMode.IMPLICIT
        t.address = 0
        t.realAddress.Type = MemoryType.CPU_REG
        t.realAddress.ID = r
        Return t
    End Function

    Public Shared Function createCodeBlockFromString(ByRef r As String) As codeBlock
        If r.StartsWith("BLOCK") Then
            Dim b As New block
            b.loadFromString(r)
            Return b
        Else
            Return instruction.createInstructionFromString(r)
        End If
    End Function

    Public Shared Function loadMemoryTargetFromString(ByRef r As String) As memoryTarget
        Dim t As memoryTarget
        Dim s() As String = r.Replace("[", "").Replace("]", "").Split(":"c)
        Select Case s(0).Trim
            Case "IMPLICIT"
                t.addrMode = AddressingMode.IMPLICIT
            Case "ACCUMULATOR"
                t.addrMode = AddressingMode.ACCUMULATOR
            Case "IMMEDIATE"
                t.addrMode = AddressingMode.IMMEDIATE
            Case "ZERO_PAGE"
                t.addrMode = AddressingMode.ZERO_PAGE
            Case "ZERO_PAGE_X"
                t.addrMode = AddressingMode.ZERO_PAGE_INDEXED_X
            Case "ZERO_PAGE_Y"
                t.addrMode = AddressingMode.ZERO_PAGE_INDEXED_Y
            Case "ABSOLUTE"
                t.addrMode = AddressingMode.ABSOLUTE
            Case "ABSOLUTE_X"
                t.addrMode = AddressingMode.ABSOLUTE_INDEXED_X
            Case "ABSOLUTE_Y"
                t.addrMode = AddressingMode.ABSOLUTE_INDEXED_Y
            Case "INDIRECT"
                t.addrMode = AddressingMode.INDIRECT
            Case "INDIRECT_X"
                t.addrMode = AddressingMode.INDEXED_INDIRECT_X
            Case "INDIRECT_Y"
                t.addrMode = AddressingMode.INDIRECT_INDEXED_Y
            Case "RELATIVE"
                t.addrMode = AddressingMode.RELATIVE
        End Select
        t.address = Convert.ToUInt16(s(1).Trim, 16)
        Select Case s(2).Trim
            Case "DISABLED"
                t.realAddress.Type = MemoryType.DISABLED
            Case "RANDOM"
                t.realAddress.Type = MemoryType.RANDOM
            Case "INIT"
                t.realAddress.Type = MemoryType.INIT
            Case "APU_REG"
                t.realAddress.Type = MemoryType.APU_REG
            Case "CPU_REG"
                t.realAddress.Type = MemoryType.CPU_REG
            Case "PPU_REG"
                t.realAddress.Type = MemoryType.PPU_REG
            Case "RAM"
                t.realAddress.Type = MemoryType.RAM
            Case "CHR_ROM"
                t.realAddress.Type = MemoryType.CHR_ROM
            Case "CHR_RAM"
                t.realAddress.Type = MemoryType.CHR_RAM
            Case "CHR_NVRAM"
                t.realAddress.Type = MemoryType.CHR_NVRAM
            Case "PRG_ROM"
                t.realAddress.Type = MemoryType.PRG_ROM
            Case "PRG_RAM"
                t.realAddress.Type = MemoryType.PRG_RAM
            Case "PRG_NVRAM"
                t.realAddress.Type = MemoryType.PRG_NVRAM
        End Select
        t.realAddress.ID = Convert.ToInt32(s(3).Trim, 16)
        Return t
    End Function

    Public Shared Function saveMemoryTargetToString(ByRef t As memoryTarget) As String
        Dim s As String = "["
        Select Case t.addrMode
            Case AddressingMode.IMPLICIT
                s &= "IMPLICIT"
            Case AddressingMode.ACCUMULATOR
                s &= "ACCUMULATOR"
            Case AddressingMode.IMMEDIATE
                s &= "IMMEDIATE"
            Case AddressingMode.ZERO_PAGE
                s &= "ZERO_PAGE"
            Case AddressingMode.ZERO_PAGE_INDEXED_X
                s &= "ZERO_PAGE_X"
            Case AddressingMode.ZERO_PAGE_INDEXED_Y
                s &= "ZERO_PAGE_Y"
            Case AddressingMode.ABSOLUTE
                s &= "ABSOLUTE"
            Case AddressingMode.ABSOLUTE_INDEXED_X
                s &= "ABSOLUTE_X"
            Case AddressingMode.ABSOLUTE_INDEXED_Y
                s &= "ABSOLUTE_Y"
            Case AddressingMode.INDIRECT
                s &= "INDIRECT"
            Case AddressingMode.INDEXED_INDIRECT_X
                s &= "INDIRECT_X"
            Case AddressingMode.INDIRECT_INDEXED_Y
                s &= "INDIRECT_Y"
            Case AddressingMode.RELATIVE
                s &= "RELATIVE"
        End Select
        s &= ":" & addressToHexStr(t.address) & ":"
        Select Case t.realAddress.Type
            Case MemoryType.DISABLED
                s &= "DISABLED"
            Case MemoryType.RANDOM
                s &= "RANDOM"
            Case MemoryType.INIT
                s &= "INIT"
            Case MemoryType.APU_REG
                s &= "APU_REG"
            Case MemoryType.CPU_REG
                s &= "CPU_REG"
            Case MemoryType.PPU_REG
                s &= "PPU_REG"
            Case MemoryType.RAM
                s &= "RAM"
            Case MemoryType.CHR_ROM
                s &= "CHR_ROM"
            Case MemoryType.CHR_RAM
                s &= "CHR_RAM"
            Case MemoryType.CHR_NVRAM
                s &= "CHR_NVRAM"
            Case MemoryType.PRG_ROM
                s &= "PRG_ROM"
            Case MemoryType.PRG_RAM
                s &= "PRG_RAM"
            Case MemoryType.PRG_NVRAM
                s &= "PRG_NVRAM"
        End Select
        s &= ":" & realAddressToHexStr(t.realAddress.ID) & "]"

        Return s
    End Function

    Public Shared Function printMemoryTargetToCode(ByRef t As memoryTarget, isRead As Boolean) As String
        Dim s As String = ""
        Select Case t.addrMode
            Case AddressingMode.IMPLICIT
                Select Case t.realAddress.ID
                    Case CpuRegister.a
                        s &= "a"
                    Case CpuRegister.x
                        s &= "x"
                    Case CpuRegister.y
                        s &= "y"
                    Case CpuRegister.s
                        s &= "s"
                End Select
            Case AddressingMode.ACCUMULATOR
                s &= "a"
            Case AddressingMode.IMMEDIATE
                s &= "0x" & t.realAddress.ID.ToString("X2")
            Case AddressingMode.ZERO_PAGE, AddressingMode.ABSOLUTE, AddressingMode.ZERO_PAGE_INDEXED_X, AddressingMode.ZERO_PAGE_INDEXED_Y,
                AddressingMode.ABSOLUTE_INDEXED_X, AddressingMode.ABSOLUTE_INDEXED_Y, AddressingMode.INDEXED_INDIRECT_X, AddressingMode.INDIRECT_INDEXED_Y
                If isRead Then
                    s &= "myMapper->readCPU(" & printMemoryTargetAsAddress(t) & ")"
                Else
                    s &= "myMapper->writeCPU(" & printMemoryTargetAsAddress(t) & ", "
                End If
        End Select
        Return s
    End Function

    Public Shared Function printMemoryTargetAsAddress(ByRef t As memoryTarget) As String
        Dim s As String = ""
        Select Case t.addrMode
            Case AddressingMode.ZERO_PAGE, AddressingMode.ABSOLUTE
                s &= "0x" & t.address.ToString("X4")
            Case AddressingMode.ZERO_PAGE_INDEXED_X
                s &= "(" & "0x" & t.address.ToString("X4") & " + x) & 0x00ff"
            Case AddressingMode.ZERO_PAGE_INDEXED_Y
                s &= "(" & "0x" & t.address.ToString("X4") & " + y) & 0x00ff)"
            Case AddressingMode.ABSOLUTE_INDEXED_X
                s &= "0x" & t.address.ToString("X4") & " + x"
            Case AddressingMode.ABSOLUTE_INDEXED_Y
                s &= "0x" & t.address.ToString("X4") & " + y"
            Case AddressingMode.INDEXED_INDIRECT_X
                s &= "myMapper->readCPU((0x" & t.address.ToString("X4") & " + x) & 0x00ff) + (myMapper->readCPU((0x" & t.address.ToString("X4") & " + x + 1) & 0x00ff) << 8)"
            Case AddressingMode.INDIRECT_INDEXED_Y
                s &= "myMapper->readCPU(0x" & t.address.ToString("X4") & ") + (myMapper->readCPU((0x" & t.address.ToString("X4") & " + 1) & 0x00ff) << 8) + y"
        End Select
        Return s
    End Function


    Public MustOverride Function isBlock() As Boolean
    Public MustOverride Function getRequiredMemoryTarget() As List(Of memoryTarget)
    Public MustOverride Sub getRequiredFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)
    Public MustOverride Function getOverwrittenMemoryTarget() As List(Of memoryTarget)
    Public MustOverride Sub getOverwrittemFlags(ByRef c As Boolean, ByRef z As Boolean, ByRef i As Boolean, ByRef d As Boolean, ByRef v As Boolean, ByRef n As Boolean)

    Public MustOverride Sub loadFromString(ByRef r As String)

    Public MustOverride Function saveToString() As String

    Public MustOverride Function printToCode(tabStr As String) As String
End Class
