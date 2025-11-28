Imports System.IO
Enum NametableArrangementType
    VERTICAL
    HORIZONTAL
End Enum

Enum TVSystemType
    NTSC
    PAL
    MULTI
    DENDY
End Enum

Enum ConsoleType
    NES_FC
    VS_SYSTEM
    PLAYCHOICE10
    EXTENDED
End Enum

Public Enum PrgByteType
    CODE_HEAD
    CODE_TAIL
    DATA
    LOCKED_DATA
    ADDRESS
    INTERRUPT_VECTOR
    UNKNOWN
    PEEK
End Enum

Module rom
    Private prgROMSize As UInt32
    Private chrROMSize As UInt32
    Private prgRAMSize As UInt32
    Private prgNVRAMSize As UInt32
    Private chrRAMSize As UInt32
    Private chrNVRAMSize As UInt32

    Private console As ConsoleType
    Private mapperNumber As UInt16
    Private subMapper As Byte
    Private tvSystems As TVSystemType

    Private nametableArrangement As NametableArrangementType
    Private hasPersistentMemory As Boolean
    Private hasTrainer As Boolean
    Private useAlternativeNametableLayout As Boolean
    Private useNES2FormatHeader As Boolean

    Public prgROM() As Byte
    Public chrROM() As Byte
    Public trainer() As Byte

    Public prgLOG() As PrgByteType

    Private objMapper As mapper

    Public Function readRomFile(pFilePath) As Boolean
        'check file path is valid
        If Not File.Exists(pFilePath) Then
            MsgBox("Error: Invalid file path")
            Return False
        End If

        'open file
        Dim fs As FileStream = File.Open(pFilePath, FileMode.Open)
        'minimum size is 16 bytes header + 16384 bytes prg rom
        If fs.Length < 16400 Then
            MsgBox("Error: File too small")
            fs.Close()
            Return False
        End If

        'read header
        Dim header(15) As Byte
        fs.Read(header, 0, 16)
        'check valid header
        If header(0) <> &H4E Or header(1) <> &H45 Or header(2) <> &H53 Or header(3) <> &H1A Then
            MsgBox("Error: Invalid file format")
            fs.Close()
            Return False
        End If

        nametableArrangement = IIf(isBitSet(header(6), 0), NametableArrangementType.HORIZONTAL, NametableArrangementType.VERTICAL)
        hasPersistentMemory = isBitSet(header(6), 1)
        hasTrainer = isBitSet(header(6), 2)
        useAlternativeNametableLayout = isBitSet(header(6), 3)

        Dim mapperLower As UInt16 = (header(6) >> 4) And &HF
        Dim mapperUpper As UInt16 = header(7) And &HF0

        useNES2FormatHeader = isBitSet(header(7), 3) And Not isBitSet(header(7), 2)
        If useNES2FormatHeader Then
            Dim mapperUpper2 As UInt16 = (CInt(header(8)) << 8) And &HF00
            mapperNumber = mapperUpper2 Or mapperUpper Or mapperLower
            subMapper = (header(8) >> 4) And &HF
            If isBitSet(header(7), 1) Then
                console = IIf(isBitSet(header(7), 0), ConsoleType.VS_SYSTEM, ConsoleType.NES_FC)
            Else
                console = IIf(isBitSet(header(7), 0), ConsoleType.EXTENDED, ConsoleType.PLAYCHOICE10)
            End If
            If (header(9) And &HF) = &HF Then
                prgROMSize = (2 ^ ((header(4) >> 2) And &H3F)) * ((header(4) And &H3) * 2 + 1)
            Else
                prgROMSize = (((CInt(header(9)) << 8) And &HF00) Or header(4)) * 16384
            End If
            If ((header(9) >> 4) And &HF) = &HF Then
                chrROMSize = (2 ^ ((header(5) >> 2) And &H3F)) * ((header(5) And &H3) * 2 + 1)
            Else
                chrROMSize = (((CInt(header(9)) << 4) And &HF00) Or header(5)) * 8192
            End If
            If header(10) And &HF = 0 Then
                prgRAMSize = 0
            Else
                prgRAMSize = &H40 << (header(10) And &HF)
            End If
            If header(10) And &HF0 = 0 Then
                prgNVRAMSize = 0
            Else
                prgNVRAMSize = &H40 << ((header(10) >> 4) And &HF)
            End If
            If header(11) And &HF = 0 Then
                chrRAMSize = 0
            Else
                chrRAMSize = &H40 << (header(11) And &HF)
            End If
            If header(11) And &HF0 = 0 Then
                chrNVRAMSize = 0
            Else
                chrNVRAMSize = &H40 << ((header(11) >> 4) And &HF)
            End If
            If isBitSet(header(12), 1) Then
                tvSystems = IIf(isBitSet(header(12), 0), TVSystemType.DENDY, TVSystemType.MULTI)
            Else
                tvSystems = IIf(isBitSet(header(12), 0), TVSystemType.PAL, TVSystemType.NTSC)
            End If
        Else
            mapperNumber = mapperUpper Or mapperLower
            subMapper = 0
            console = IIf(isBitSet(header(7), 0), ConsoleType.VS_SYSTEM, ConsoleType.NES_FC)
            prgROMSize = header(4) * 16384
            chrROMSize = header(5) * 8192
            prgRAMSize = header(8) * 8192
            prgNVRAMSize = 0
            chrRAMSize = IIf(chrROMSize > 0, 0, 8192)
            chrNVRAMSize = 0
            tvSystems = IIf(isBitSet(header(9), 0), TVSystemType.PAL, TVSystemType.NTSC)
        End If

        'check file size valid
        Dim totalSize As Integer = 16 + IIf(hasTrainer, 512, 0) + prgROMSize + chrROMSize
        If fs.Length < totalSize Then
            MsgBox("Error: Wrong file size")
            fs.Close()
            Return False
        End If

        'read content
        If hasTrainer Then
            trainer = New Byte(511) {}
            fs.Read(trainer, 0, 512)
        End If
        prgROM = New Byte(prgROMSize - 1) {}
        fs.Read(prgROM, 0, prgROMSize)
        chrROM = New Byte(chrROMSize - 1) {}
        fs.Read(chrROM, 0, chrROMSize)
        fs.Close()
        'set up mapper
        Select Case mapperNumber
            Case 0
                objMapper = New mapper000
            Case 1
                objMapper = New mapper001
            Case Else
                MsgBox("Unsupported mapper")
                Return False
        End Select
        objMapper.setMemorySize(MemoryType.PRG_ROM, prgROMSize)
        objMapper.setMemorySize(MemoryType.PRG_RAM, prgRAMSize)
        objMapper.setMemorySize(MemoryType.PRG_NVRAM, prgNVRAMSize)
        objMapper.setMemorySize(MemoryType.CHR_ROM, chrROMSize)
        objMapper.setMemorySize(MemoryType.CHR_RAM, chrRAMSize)
        objMapper.setMemorySize(MemoryType.CHR_NVRAM, chrNVRAMSize)

        'set up log
        prgLOG = New PrgByteType(prgROMSize - 1) {}
        For i As Integer = 0 To prgROMSize - 1
            prgLOG(i) = PrgByteType.UNKNOWN
        Next

        Return True
    End Function

    Public Sub init()

    End Sub

    Private Function addressKnown(pAddress As UInt32) As Boolean
        Return prgLOG(pAddress) <> PrgByteType.UNKNOWN
    End Function

    Public Function readAddress(pAddress As UInt16, pReadType As PrgByteType) As memoryByte
        'convert to actual address
        Dim realAddress As memoryID
        realAddress = objMapper.getActualAddress(pAddress)
        Dim result As memoryByte
        result.source = realAddress

        If realAddress.Type = MemoryType.PRG_ROM Then
            result.known = addressKnown(realAddress.ID)
            result.currentValue = prgROM(realAddress.ID)
            result.unchanged = True
            result.currentUsage = prgLOG(realAddress.ID)
            If (pReadType <> PrgByteType.PEEK And (prgLOG(realAddress.ID) = PrgByteType.UNKNOWN) _
                Or (prgLOG(realAddress.ID) <> PrgByteType.CODE_HEAD And prgLOG(realAddress.ID) <> PrgByteType.LOCKED_DATA And pReadType = PrgByteType.CODE_HEAD)) Then
                prgLOG(realAddress.ID) = pReadType
            End If
        End If

        Return result
    End Function

    Public Sub writeAddress(pAddress As UInt16, pV As memoryByte)
        objMapper.write(pAddress, pV.currentValue)
    End Sub


    Public Sub setupMapperConfig(c As UInt64)
        objMapper.setMapperConfig(c)
    End Sub

    Public Function getMapperConfig() As UInt64
        Return objMapper.getMapperConfig
    End Function

    Public Function getPrgROMSize() As UInt32
        Return prgROMSize
    End Function

    Public Sub resetPrgLOG()
        For i As Integer = 0 To prgROMSize - 1
            prgLOG(i) = PrgByteType.UNKNOWN
        Next
    End Sub

End Module
