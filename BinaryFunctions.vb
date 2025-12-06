Module BinaryFunctions
    Public bitMask() As Byte = {&H1, &H2, &H4, &H8, &H10, &H20, &H40, &H80}
    Public Function isBitSet(pValue As Byte, pCheckBit As Byte) As Boolean
        Return (pValue And bitMask(pCheckBit)) <> 0
    End Function

    Public Function setBit(pValue As Byte, pSetBit As Byte) As Byte
        Return pValue Or bitMask(pSetBit)
    End Function

    Public Function byteSubtract(pValue As Byte, pSubAmt As Byte) As Byte
        Dim t As UInt16 = pValue
        If pValue < pSubAmt Then
            t = t + &H100
        End If
        t -= pSubAmt
        Return t
    End Function

    Public Function byteAdd(pValue As Byte, pAddAmt As Byte) As Byte
        Dim t As UInt16 = pValue
        t += pAddAmt
        Return t And &HFF
    End Function


    Public Function byteToHexStr(v As Byte) As String
        Return v.ToString("X2")
    End Function

    Public Function addressToHexStr(a As UInt16) As String
        Return a.ToString("X4")
    End Function

    Public Function realAddressToHexStr(a As UInt32) As String
        Return a.ToString("X6")
    End Function

    Public Function realAddressListToHexStr(l As List(Of UInt32)) As String
        Dim r As String = ""
        For Each a As UInt32 In l
            If r <> "" Then
                r = r & ","
            End If
            r = r & realAddressToHexStr(a)
        Next
        Return r
    End Function

    Public Function hexStrToRealAddressList(s As String) As List(Of UInt32)
        Dim r As New List(Of UInt32)
        Dim t() As String = Split(s, ",")
        For Each a As String In t
            If a <> "" Then
                r.Add(Convert.ToUInt32(a, 16))
            End If
        Next
        Return r
    End Function
End Module
