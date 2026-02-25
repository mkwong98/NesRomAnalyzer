Public MustInherit Class instPBranch
    Inherits instruction

    Public useFlag As FlagID
    Public flagIsSet As Boolean
    Public branchAddress As UInt16
    Public branchToAddress As New List(Of UInt32)
    Public branchToFixedAddress As New List(Of UInt32)

End Class
