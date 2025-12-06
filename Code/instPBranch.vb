Public MustInherit Class instPBranch
    Inherits instruction

    Public useFlag As FlagID
    Public flagIsSet As Boolean
    Public branchToAddress As New List(Of UInt32)

End Class
