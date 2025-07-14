Public Class instStack
    Inherits instruction

    Public regToKeep As CpuRegister
    Public isPush As Boolean 'false = pull

    Public Sub New()
        type = InstructionType.STACK
    End Sub

    Public Overrides Sub loadInstructionContentFromString(ByRef r As String)
        Dim s() As String = r.Split(";"c)
        Select Case s(0).Trim.ToUpper
            Case "A"
                regToKeep = CpuRegister.a
            Case "P"
                regToKeep = CpuRegister.p
        End Select
        Select Case s(1).Trim.ToUpper
            Case "PUSH"
                isPush = True
            Case "PULL"
                isPush = False
        End Select
    End Sub

    Public Overrides Function saveInstructionContentToString() As String
        Dim s As String = ""
        Select Case regToKeep
            Case CpuRegister.a
                s &= "A"
            Case CpuRegister.p
                s &= "P"
        End Select
        s &= ";"
        If isPush Then
            s &= "PUSH"
        Else
            s &= "PULL"
        End If
        Return s
    End Function

    Public Overrides Function printToCode(tabStr As String) As String
        If isPush Then
            If regToKeep = CpuRegister.a Then
                Return printLabel() & tabStr & "opPHA();" & vbCrLf
            Else
                Return printLabel() & tabStr & "pushStatus();" & vbCrLf
            End If

        Else
            Dim s As String = printLabel()
            If regToKeep = CpuRegister.a Then
                s &= tabStr & "opPLA();" & vbCrLf
            Else
                s &= tabStr & "popStatus();" & vbCrLf
            End If
            Return s
        End If
    End Function
End Class
