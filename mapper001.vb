Public Class mapper001
    Inherits mapper

    Private prgROMSize As UInt32

    Public Sub New()
        Dim r As New bankRange With {
            .name = "Mode 0 BANK",
            .mode = "Mode 0",
            .id = "0",
            .startBank = &H8000,
            .size = &H8000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 1 BANK",
            .mode = "Mode 1",
            .id = "1",
            .startBank = &HC000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 2 BANK",
            .mode = "Mode 2",
            .id = "2",
            .startBank = &H8000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 1 FIX",
            .mode = "Mode 1",
            .startBank = &H8000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        fixedBanks.Add(r)

        r = New bankRange With {
            .name = "Mode 2 FIX",
            .mode = "Mode 2",
            .startBank = &HC000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        fixedBanks.Add(r)

        modes.Add("Mode 0")
        modes.Add("Mode 1")
        modes.Add("Mode 2")
    End Sub

    Public Overrides Function getDefaultMapping() As String
        Dim r As New bankMapping With {
            .id = "2",
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
            For Each useMode As String In modes
                'Check if any bank in this mode is used in the current config
                Dim inUse As Boolean = False
                For Each b As bankRange In banks
                    If b.mode = useMode Then
                        For Each mapping In config
                            If b.id = mapping.id Then
                                inUse = True
                                Exit For
                            End If
                        Next
                    End If
                Next

                If inUse Then
                    For Each b As bankRange In banks
                        If b.mode = useMode Then
                            If address >= b.startBank And address < b.startBank + b.size Then
                                For Each mapping In config
                                    If b.id = mapping.id Then
                                        Dim r As memoryID
                                        r.Type = MemoryType.PRG_ROM
                                        r.ID = mapping.mappedAddress + (address - b.startBank)
                                        r.config = switchBank(configS, config)
                                        r.address = address
                                        r.bank = b.id
                                        result.Add(r)
                                    End If
                                Next
                            End If
                        End If
                    Next

                    For Each b As bankRange In fixedBanks
                        If b.mode = useMode Then
                            If address >= b.startBank And address < b.startBank + b.size Then
                                For Each mappedAddress As UInt32 In b.mappedAddresses
                                    Dim r As memoryID
                                    r.Type = MemoryType.PRG_ROM
                                    r.ID = mappedAddress + (address - b.startBank)
                                    r.config = configS
                                    r.address = address
                                    r.bank = ""
                                    result.Add(r)
                                Next
                            End If
                        End If
                    Next
                End If
            Next
        End If

        Return result
    End Function

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt32)
        If type = MemoryType.PRG_ROM Then
            prgROMSize = size
            For Each b As bankRange In fixedBanks
                If b.mode = "Mode 1" Then
                    b.mappedAddresses.Clear()
                    b.mappedAddresses.Add(0)
                ElseIf b.mode = "Mode 2" Then
                    b.mappedAddresses.Clear()
                    b.mappedAddresses.Add(prgROMSize - b.size)
                End If
            Next
            For Each b As bankRange In banks
                b.mappedAddresses.Clear()
                Dim rAddr As UInt32
                If b.mode = "Mode 0" Then
                    rAddr = 0
                    While rAddr < prgROMSize
                        b.mappedAddresses.Add(rAddr)
                        rAddr += b.size
                    End While
                ElseIf b.mode = "Mode 1" Then
                    rAddr = b.size
                    While rAddr < prgROMSize
                        b.mappedAddresses.Add(rAddr)
                        rAddr += b.size
                    End While
                ElseIf b.mode = "Mode 2" Then
                    rAddr = 0
                    While rAddr < prgROMSize - b.size
                        b.mappedAddresses.Add(rAddr)
                        rAddr += b.size
                    End While
                End If
            Next

        End If
    End Sub


End Class
