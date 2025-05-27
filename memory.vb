Imports Windows.Win32.System

Public Enum MemoryType
    INIT
    RANDOM
    RAM
    CPU_REG
    PPU_REG
    APU_REG
    CHR_RAM
    CHR_NVRAM
    CHR_ROM
    PRG_RAM
    PRG_NVRAM
    PRG_ROM
    DISABLED
End Enum

Public Structure memoryID
    Public Type As MemoryType
    Public ID As UInt32
End Structure


Public Structure memoryByte
    Public unchanged As Boolean
    Public currentValue As Byte
    Public source As memoryID
    Public currentUsage As PrgByteType
    Public known As Boolean
End Structure

Module memory
    Private internalRAM(&H7FF) As memoryByte

    Public Sub powerOn()
        For i As UInt16 = 0 To &H7FF
            internalRAM(i).source.Type = MemoryType.RANDOM
            internalRAM(i).currentValue = CByte(Int(VBMath.Rnd() * 256))
        Next
    End Sub

    Public Function read(pAddress As UInt16, pUsage As PrgByteType, pAccess As UInt32) As memoryByte
        Dim tMem As memoryByte
        If pAddress < &H2000 Then
            tMem.source.Type = MemoryType.RAM
            tMem.source.ID = pAddress And &H7FF
            tMem.currentValue = internalRAM(tMem.source.ID).currentValue
            tMem.unchanged = internalRAM(tMem.source.ID).unchanged
        ElseIf pAddress < &H4000 Then
            tMem.source.Type = MemoryType.PPU_REG
            tMem.source.ID = pAddress And &H7
            tMem.unchanged = False
        ElseIf pAddress < &H4018 Then
            tMem.source.Type = MemoryType.APU_REG
            tMem.source.ID = pAddress And &H17
            tMem.unchanged = False
        ElseIf pAddress < &H4020 Then
            tMem.source.Type = MemoryType.DISABLED
            tMem.source.ID = pAddress
            tMem.unchanged = False
        Else
            tMem = readAddress(pAddress, pUsage, pAccess)
        End If
        Return tMem
    End Function

    Public Function readAsAddress(pAddress As UInt16, pUsage As PrgByteType, pAccess As UInt32) As UInt16
        Dim tMem As memoryByte = read(pAddress, pUsage, pAccess)
        Dim tMem2 As memoryByte = read(pAddress + 1, pUsage, pAccess)
        Return CInt(tMem2.currentValue) << 8 Or tMem.currentValue
    End Function

    Public Sub write(pAddress As UInt16, pV As memoryByte)
        If pAddress < &H2000 Then
            internalRAM(pAddress And &H7FF) = pV
        ElseIf pAddress >= &H4020 Then
            writeAddress(pAddress, pV)
        End If
    End Sub

    Public Function getMemoryName(m As memoryID) As String
        Dim r As String = ""
        Select Case m.Type
            Case MemoryType.RAM
                Return "RAM" & addressToHexStr(m.ID)
            Case MemoryType.PPU_REG
                Select Case m.ID
                    Case 0
                        Return "PPUCTRL"
                    Case 1
                        Return "PPUMASK"
                    Case 2
                        Return "PPUSTATUS"
                    Case 3
                        Return "OAMADDR"
                    Case 4
                        Return "OAMDATA"
                    Case 5
                        Return "PPUSCROLL"
                    Case 6
                        Return "PPUADDR"
                    Case 7
                        Return "PPUDATA"
                End Select
            Case MemoryType.APU_REG
                Select Case m.ID
                    Case &H0
                        Return "SQ1_VOL"
                    Case &H1
                        Return "SQ1_SWEEP"
                    Case &H2
                        Return "SQ1_LO"
                    Case &H3
                        Return "SQ1_HI"
                    Case &H4
                        Return "SQ2_VOL"
                    Case &H5
                        Return "SQ2_SWEEP"
                    Case &H6
                        Return "SQ2_LO"
                    Case &H7
                        Return "SQ2_HI"
                    Case &H8
                        Return "TRI_LINEAR"
                    Case &H9
                        Return "TRI_UNUSED"
                    Case &HA
                        Return "TRI_LO"
                    Case &HB
                        Return "TRI_HI"
                    Case &HC
                        Return "NOISE_VOL"
                    Case &HD
                        Return "NOISE_UNUSED"
                    Case &HE
                        Return "NOISE_LO"
                    Case &HF
                        Return "NOISE_HI"
                    Case &H10
                        Return "DMC_FREQ"
                    Case &H11
                        Return "DMC_RAW"
                    Case &H12
                        Return "DMC_START"
                    Case &H13
                        Return "DMC_LEN"
                    Case &H14
                        Return "OAMDMA"
                    Case &H15
                        Return "SND_CHN"
                    Case &H16
                        Return "JOY1"
                    Case &H17
                        Return "JOY2"
                End Select

            Case MemoryType.DISABLED
                Return "DISABLED"
            Case MemoryType.PRG_ROM
                Return "PRG_ROM" & realAddressToHexStr(m.ID)
            Case MemoryType.PRG_RAM
                Return "PRG_RAM" & realAddressToHexStr(m.ID)

        End Select
        Return ""
    End Function
End Module
