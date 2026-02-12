Public Class mapper002
    Inherits mapper

    Private prgROMSize As UInt32

    Public Sub New()
        Dim r As New bankRange With {
            .name = "SWITCHABLE",
            .mode = "-",
            .id = "0",
            .startBank = &H8000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "FIXED",
            .mode = "-",
            .startBank = &HC000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        fixedBanks.Add(r)

        modes.Add("-")
    End Sub

    Public Overrides Function getDefaultMapping() As String
        Dim r As New bankMapping With {
            .id = "0",
            .mappedAddress = 0
        }
        Dim mappings As New List(Of bankMapping)
        mappings.Add(r)
        Return convertRangesToMappingString(convertMappingsToRanges(mappings))
    End Function

    Public Overrides Function getActualAddress(address As UShort, configS As String) As List(Of memoryID)
        Dim config As List(Of bankMapping) = convertMappingStringToList(configS)
        Dim result As New List(Of memoryID)
        If config.Count > 0 Then
            If address >= banks(0).startBank And address < banks(0).startBank + banks(0).size Then
                For Each mapping In config
                    Dim r As memoryID
                    r.Type = MemoryType.PRG_ROM
                    r.ID = mapping.mappedAddress + (address - banks(0).startBank)
                    r.config = switchBank(configS, config)
                    r.address = address
                    r.bank = banks(0).id
                    result.Add(r)
                Next
            End If
        End If
        If address >= fixedBanks(0).startBank And address < fixedBanks(0).startBank + fixedBanks(0).size Then
            For Each mappedAddress As UInt32 In fixedBanks(0).mappedAddresses
                Dim r As memoryID
                r.Type = MemoryType.PRG_ROM
                r.ID = mappedAddress + (address - fixedBanks(0).startBank)
                r.config = configS
                r.address = address
                r.bank = ""
                result.Add(r)
            Next
        End If

        Return result
    End Function

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt32)
        If type = MemoryType.PRG_ROM Then
            prgROMSize = size
            fixedBanks(0).mappedAddresses.Add(prgROMSize - fixedBanks(0).size)
        End If
    End Sub


End Class
