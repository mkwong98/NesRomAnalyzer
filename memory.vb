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
    Public address As UInt16
    Public config As bankConfig
End Structure


Public Structure memoryByte
    Public unchanged As Boolean
    Public currentValue As Byte
    Public source As memoryID
    Public currentUsage As PrgByteType
    Public known As Boolean
End Structure

Module memory

    Public Function read(pAddress As UInt16, pUsage As PrgByteType, pConfig As bankConfig) As List(Of memoryByte)
        Dim result As New List(Of memoryByte)
        Dim tMem As memoryByte
        If pAddress < &H2000 Then
            tMem.source.Type = MemoryType.RAM
            tMem.source.ID = pAddress And &H7FF
            result.Add(tMem)
        ElseIf pAddress < &H4000 Then
            tMem.source.Type = MemoryType.PPU_REG
            tMem.source.ID = pAddress And &H7
            tMem.unchanged = False
            result.Add(tMem)
        ElseIf pAddress < &H4018 Then
            tMem.source.Type = MemoryType.APU_REG
            tMem.source.ID = pAddress And &H17
            tMem.unchanged = False
            result.Add(tMem)
        ElseIf pAddress < &H4020 Then
            tMem.source.Type = MemoryType.DISABLED
            tMem.source.ID = pAddress
            tMem.unchanged = False
            result.Add(tMem)
        Else
            result = getMappedMemoryBytes(pAddress, pUsage, pConfig)
        End If
        Return result
    End Function

    Public Function readAsAddress(pAddress As UInt16, pUsage As PrgByteType, pConfig As bankConfig) As List(Of memoryID)
        Dim result As New List(Of memoryID)
        Dim tMem As List(Of memoryByte) = read(pAddress, pUsage, pConfig)
        Dim tMB As memoryID
        For Each mb As memoryByte In tMem
            Dim val As Byte = rom.prgROM(mb.source.ID + 1)
            tMB = mb.source
            tMB.address = CInt(val) << 8 Or mb.currentValue
            result.Add(tMB)
        Next
        Return result
    End Function

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
