Imports System.IO
Public Class frmMain
    Private fileRead As Boolean = False

    Public Sub New()

        ' 設計工具需要此呼叫。
        InitializeComponent()

        ' 在 InitializeComponent() 呼叫之後加入所有初始設定。
        VBMath.Randomize()
        analyzer.frm = Me
    End Sub

    Private Sub btnFilePicker_Click(sender As Object, e As EventArgs) Handles btnFilePicker.Click
        If ofdRomFile.ShowDialog = DialogResult.OK Then
            txtFilePath.Text = ofdRomFile.FileName
            filePathChanged()
        End If
    End Sub

    Private Sub btnProcess_Click(sender As Object, e As EventArgs) Handles btnProcess.Click
        If fileRead Then
            lsvOutput.Items.Clear()
            lsvIndirectJmp.Items.Clear()
            cboTargetAddress.Text = ""
            cboTargetAddress.Items.Clear()
            txtAnaCode.Text = ""
            analyzer.start()
        End If
    End Sub

    Private Sub btnExportBasic_Click(sender As Object, e As EventArgs) Handles btnExportBasic.Click
        Dim r = ""
        For Each h As ColumnHeader In lsvOutput.Columns
            r &= h.Text & ","
        Next
        r &= vbCrLf
        For Each i As ListViewItem In lsvOutput.Items
            For Each j As ListViewItem.ListViewSubItem In i.SubItems
                r &= vbTab & j.Text & ","
            Next
            r &= vbCrLf
        Next

        If sfdExportBasic.ShowDialog = DialogResult.OK Then
            Dim fs = My.Computer.FileSystem.OpenTextFileWriter(sfdExportBasic.FileName, False)
            fs.Write(r)
            fs.Close()
        End If

    End Sub

    Private Sub btnExportAna_Click(sender As Object, e As EventArgs) Handles btnExportAna.Click
        If sfdAnaFile.ShowDialog = DialogResult.OK Then
            Dim fs = My.Computer.FileSystem.OpenTextFileWriter(sfdAnaFile.FileName, False)
            fs.Write(txtAnaCode.Text)
            fs.Close()
        End If
    End Sub

    Private Sub btnLoadAna_Click(sender As Object, e As EventArgs) Handles btnLoadAna.Click
        If ofdAnaFile.ShowDialog = DialogResult.OK Then
            Dim fs = My.Computer.FileSystem.OpenTextFileReader(ofdAnaFile.FileName)
            txtAnaCode.Text = fs.ReadToEnd()
            fs.Close()
            analyzer.loadBlocksFromString(txtAnaCode.Text)
            console.init()
        End If
    End Sub

    Private Sub btnAnalyse_Click(sender As Object, e As EventArgs) Handles btnAnalyse.Click
        analyse()
    End Sub

    Private Sub btnGenerate_Click(sender As Object, e As EventArgs) Handles btnGenerate.Click
        generate()
    End Sub

    Private Sub lsvIndirectJmp_ItemSelectionChanged(sender As Object, e As ListViewItemSelectionChangedEventArgs) Handles lsvIndirectJmp.ItemSelectionChanged
        If e.IsSelected Then
            Dim addresses = Split(e.Item.SubItems(2).Text, ",")
            cboTargetAddress.Items.Clear
            cboTargetAddress.Items.AddRange(addresses)
        End If
    End Sub

    Private Sub btnAddTargetAddress_Click(sender As Object, e As EventArgs) Handles btnAddTargetAddress.Click
        If cboTargetAddress.Text <> "" Then
            If cboTargetAddress.Items.Contains(cboTargetAddress.Text) Then
                MsgBox("This target address is in the list")
                Return
            End If
            cboTargetAddress.Items.Add(cboTargetAddress.Text)
            updateIndirectJumpTarget
        End If
    End Sub

    Private Sub btnRemoveTargetAddress_Click(sender As Object, e As EventArgs) Handles btnRemoveTargetAddress.Click
        If cboTargetAddress.Text <> "" Then
            cboTargetAddress.Items.Remove(cboTargetAddress.Text)
            updateIndirectJumpTarget
        End If
    End Sub

    Private Sub updateIndirectJumpTarget()
        If lsvIndirectJmp.SelectedItems.Count = 0 Then Return
        Dim itx As ListViewItem = lsvIndirectJmp.SelectedItems(0)
        Dim r As String = ""
        For Each a As String In cboTargetAddress.Items
            If r <> "" Then
                r = r & ","
            End If
            r = r & a
        Next
        For Each i As instJump In indirectJmpList
            If realAddressToHexStr(i.realAddress) = itx.Text Then
                i.readIndirectJumpTargetString(r)
                itx.SubItems(2).Text = r
            End If
        Next
    End Sub

    Private Sub btnRunIndirect_Click(sender As Object, e As EventArgs) Handles btnRunIndirect.Click
        analyzer.startIndirect()
    End Sub

    Private Sub lsvBanks_ItemSelectionChanged(sender As Object, e As ListViewItemSelectionChangedEventArgs) Handles lsvBanks.ItemSelectionChanged
        If e.IsSelected Then
            Dim addresses As String() = Split(e.Item.SubItems(3).Text, ",")
            cboMapToAddress.Items.Clear()
            cboMapToAddress.Items.AddRange(addresses)
        End If
    End Sub

    Private Sub btnAddMapping_Click(sender As Object, e As EventArgs) Handles btnAddMapping.Click
        If cboMapToAddress.Text <> "" Then
            If cboMapToAddress.Items.Contains(cboMapToAddress.Text) Then
                MsgBox("This mapping address is in the list")
                Return
            End If
            cboMapToAddress.Items.Add(cboMapToAddress.Text)
            updateMappingTarget()
        End If
    End Sub

    Private Sub btnRemoveMapping_Click(sender As Object, e As EventArgs) Handles btnRemoveMapping.Click
        If cboMapToAddress.Text <> "" Then
            cboMapToAddress.Items.Remove(cboMapToAddress.Text)
            updateMappingTarget()
        End If
    End Sub

    Private Sub updateMappingTarget()
        If lsvBanks.SelectedItems.Count = 0 Then Return
        Dim itx As ListViewItem = lsvBanks.SelectedItems(0)
        itx.SubItems(3).Text = ""
        For Each a As String In cboMapToAddress.Items
            If itx.SubItems(3).Text <> "" Then
                itx.SubItems(3).Text &= ","
            End If
            itx.SubItems(3).Text &= a
        Next
    End Sub

    Private Sub txtFilePath_TextChanged(sender As Object, e As EventArgs) Handles txtFilePath.TextChanged
        filePathChanged()
    End Sub

    Sub filePathChanged()
        fileRead = readRomFile(txtFilePath.Text)
        If fileRead Then
            lsvIndirectJmp.Items.Clear()
            lsvBanks.Items.Clear()

            For Each i As bankRange In rom.getBankRanges()
                Dim itx As New ListViewItem({i.name, realAddressToHexStr(i.size), i.id})
                itx.SubItems.Add("")
                For Each addr As UInt32 In i.mappedAddresses
                    If itx.SubItems(3).Text <> "" Then
                        itx.SubItems(3).Text &= ","
                    End If
                    itx.SubItems(3).Text &= realAddressToHexStr(addr)
                Next
                lsvBanks.Items.Add(itx)
            Next
            updateMappingTarget()
        End If

    End Sub

    Private Sub btnMappingActivationAdd_Click(sender As Object, e As EventArgs) Handles btnMappingActivationAdd.Click
        Dim addresss() As String = Split(txtMappingSet.Text, ",")
        For Each addr As String In addresss
            If addr.Length > 0 Then
                Dim itx As New ListViewItem({txtMappingActivation.Text, addr})
                lsvMappingActivation.Items.Add(itx)
            End If
        Next
    End Sub

    Private Sub btnMappingActivationRemove_Click(sender As Object, e As EventArgs) Handles btnMappingActivationRemove.Click
        If lsvMappingActivation.SelectedItems.Count > 0 Then
            lsvMappingActivation.Items.Remove(lsvMappingActivation.SelectedItems(0))
        End If
    End Sub

End Class
