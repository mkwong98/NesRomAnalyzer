Public Structure mode
    Public name As String
    Public enabled As Boolean
End Structure

Public Structure bankRange
    Public name As String
    Public mode As String
    Public startBank As UInt16
    Public size As UInt16
    Public mappedAddresses As List(Of UInt32)
End Structure

Public MustInherit Class mapper
    Public modes As List(Of mode) = New List(Of mode)
    Public banks As List(Of bankRange) = New List(Of bankRange)
    Public fixedBanks As List(Of bankRange) = New List(Of bankRange)

    Public MustOverride Function getActualAddress(address As UInt16) As List(Of memoryID)

    Public MustOverride Function getActualAddressWithConfig(address As UInt16, config As memoryID) As List(Of memoryID)

    Public MustOverride Sub setMemorySize(type As MemoryType, size As UInt32)

    Public Sub setEnabledModes(pModeNames As List(Of String))
        For Each m As mode In modes
            If pModeNames.Contains(m.name) Then
                m.enabled = True
            Else
                m.enabled = False
            End If
        Next
    End Sub

    Public Sub setBankMappings(pModeName As String, pBankMappings As String)
        For Each b As bankRange In banks
            If b.mode = pModeName Then
                b.mappedAddresses.Clear()
                Dim mappings As String() = pBankMappings.Split(","c)
                For Each mapping As String In mappings
                    b.mappedAddresses.Add(Convert.ToUInt32(mapping.Trim(), 16))
                Next
            End If
        Next
    End Sub
End Class
