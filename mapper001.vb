Public Class mapper001
    Inherits mapper

    Private prgROMSize As UInt32

    Public Sub New()
        Dim r As New bankRange With {
            .name = "Mode 0 BANK",
            .mode = "0",
            .startBank = &H8000,
            .size = &H8000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 1 BANK",
            .mode = "1",
            .startBank = &HC000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 2 BANK",
            .mode = "2",
            .startBank = &H8000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        banks.Add(r)

        r = New bankRange With {
            .name = "Mode 1 FIX",
            .mode = "1",
            .startBank = &H8000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        r.mappedAddresses.Add(0)
        fixedBanks.Add(r)

        r = New bankRange With {
            .name = "Mode 2 FIX",
            .mode = "2",
            .startBank = &HC000,
            .size = &H4000,
            .mappedAddresses = New List(Of UInt32)
        }
        r.mappedAddresses.Add(prgROMSize - r.size)
        fixedBanks.Add(r)


        Dim m As New mode With {
            .name = "Mode 0",
            .enabled = True
        }
        modes.Add(m)

        m = New mode With {
            .name = "Mode 1",
            .enabled = True
        }
        modes.Add(m)

        m = New mode With {
            .name = "Mode 2",
            .enabled = True
        }
        modes.Add(m)
    End Sub

    Public Overrides Function getActualAddress(address As UShort) As List(Of memoryID)
        Dim result As New List(Of memoryID)
        For Each m As mode In modes
            If m.enabled Then
                For Each b As bankRange In banks
                    If b.mode = m.name Then
                        If address >= b.startBank And address < b.startBank + b.size Then
                            For Each mappedAddress As UInt32 In b.mappedAddresses
                                Dim r As memoryID
                                r.Type = MemoryType.PRG_ROM
                                r.ID = mappedAddress + (address - b.startBank)
                                r.mode = m.name
                                r.bank = b.name
                                r.mappedAddress = mappedAddress
                                r.address = address
                                result.Add(r)
                            Next
                        End If
                    End If
                Next

                For Each b As bankRange In fixedBanks
                    If b.mode = m.name Then
                        If address >= b.startBank And address < b.startBank + b.size Then
                            For Each mappedAddress As UInt32 In b.mappedAddresses
                                Dim r As memoryID
                                r.Type = MemoryType.PRG_ROM
                                r.ID = mappedAddress + (address - b.startBank)
                                r.mode = m.name
                                r.bank = b.name
                                r.mappedAddress = mappedAddress
                                r.address = address
                                result.Add(r)
                            Next
                        End If
                    End If
                Next
            End If
        Next
        Return result
    End Function

    Public Overrides Function getActualAddressWithConfig(address As UShort, config As memoryID) As List(Of memoryID)
        Dim result As New List(Of memoryID)
        If config.mode = "" Then
            Return getActualAddress(address)
        End If
        For Each m As mode In modes
            'must be the same mode as in config
            If m.enabled And m.name = config.mode Then
                For Each b As bankRange In banks
                    If b.mode = m.name Then
                        If address >= b.startBank And address < b.startBank + b.size Then
                            For Each mappedAddress As UInt32 In b.mappedAddresses
                                'either same bank and mapped address, or different bank and any mapped address
                                If b.name = config.bank And mappedAddress <> config.mappedAddress Then
                                    Continue For
                                End If
                                Dim r As memoryID
                                r.Type = MemoryType.PRG_ROM
                                r.ID = mappedAddress + (address - b.startBank)
                                r.mode = m.name
                                r.bank = b.name
                                r.mappedAddress = mappedAddress
                                r.address = address
                                result.Add(r)
                            Next
                        End If
                    End If
                Next

                For Each b As bankRange In fixedBanks
                    If b.mode = m.name Then
                        If address >= b.startBank And address < b.startBank + b.size Then
                            For Each mappedAddress As UInt32 In b.mappedAddresses
                                'either same bank and mapped address, or different bank and any mapped address
                                If b.name = config.bank And mappedAddress <> config.mappedAddress Then
                                    Continue For
                                End If
                                Dim r As memoryID
                                r.Type = MemoryType.PRG_ROM
                                r.ID = mappedAddress + (address - b.startBank)
                                r.mode = m.name
                                r.bank = b.name
                                r.mappedAddress = mappedAddress
                                r.address = address
                                result.Add(r)
                            Next
                        End If
                    End If
                Next
            End If
        Next
        Return result
    End Function

    Public Overrides Sub setMemorySize(type As MemoryType, size As UInt32)
        If type = MemoryType.PRG_ROM Then
            prgROMSize = size
        End If
    End Sub


End Class
