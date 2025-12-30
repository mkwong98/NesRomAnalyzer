Public Structure bankRange
    Public name As String
    Public mode As String
    Public id As String
    Public startBank As UInt32
    Public size As UInt32
    Public mappedAddresses As List(Of UInt32)
End Structure

Public Structure bankMapping
    Public id As String
    Public mappedAddress As UInt32
End Structure

Public MustInherit Class mapper
    Public modes As List(Of String) = New List(Of String)
    Public banks As List(Of bankRange) = New List(Of bankRange)
    Public fixedBanks As List(Of bankRange) = New List(Of bankRange)
    Public Overridable Function getDefaultMapping() As String
        Return ""
    End Function


    Public MustOverride Function getActualAddress(address As UInt16, config As String) As List(Of memoryID)

    Public MustOverride Sub setMemorySize(type As MemoryType, size As UInt32)

    Public Shared Function switchBank(fullMappings As String, newMapping As List(Of bankMapping)) As String
        Dim oldRanges As List(Of bankRange) = convertMappingsToRanges(convertMappingStringToList(fullMappings))
        Dim newRanges As List(Of bankRange) = convertMappingsToRanges(newMapping)
        For Each rO As bankRange In oldRanges
            Dim found As Boolean = False
            For Each rN As bankRange In newRanges
                If rO.id = rN.id Then
                    found = True
                    Exit For
                End If
            Next
            If Not found Then
                newRanges.Add(rO)
            End If
        Next
        Return convertRangesToMappingString(newRanges)
    End Function

    Public Shared Function convertMappingStringToList(fullMappings As String) As List(Of bankMapping)
        Dim banks() As String = Split(fullMappings, "+")
        Dim mapping As New List(Of bankMapping)
        For Each s As String In banks
            Dim parts() As String = Split(s, ":")
            Dim id As String = parts(0).Trim
            Dim addresses() As String = Split(parts(1), "/")
            For Each addr As String In addresses
                Dim bm As bankMapping
                bm.id = id
                bm.mappedAddress = Convert.ToUInt32(addr.Trim, 16)
                mapping.Add(bm)
            Next
        Next
        Return mapping
    End Function

    Public Shared Function convertMappingsToRanges(newMapping As List(Of bankMapping)) As List(Of bankRange)
        'Build new bank ranges
        Dim bankRanges As New List(Of bankRange)
        For Each m As bankMapping In newMapping
            Dim hasBank As Boolean = False
            For Each b As bankRange In bankRanges
                If b.id = m.id Then
                    hasBank = True
                    b.mappedAddresses.Add(m.mappedAddress)
                End If
            Next
            If Not hasBank Then
                Dim b As New bankRange With {
                    .id = m.id,
                    .mappedAddresses = New List(Of UInt32)
                }
                b.mappedAddresses.Add(m.mappedAddress)
                bankRanges.Add(b)
            End If
        Next
        Return bankRanges
    End Function

    Public Shared Function convertRangesToMappingString(ranges As List(Of bankRange)) As String
        Dim r As String = ""
        For Each br As bankRange In ranges
            If r <> "" Then
                r &= "+"
            End If
            r &= br.id & ":"
            Dim first As Boolean = True
            For Each addr As UInt32 In br.mappedAddresses
                If Not first Then
                    r &= "/"
                End If
                r &= realAddressToHexStr(addr)
                first = False
            Next
        Next
        Return r
    End Function
End Class
