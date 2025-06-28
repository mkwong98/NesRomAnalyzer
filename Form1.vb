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
            fileRead = readRomFile(txtFilePath.Text)
            lsvIndirectJmp.Items.Clear()

        End If
    End Sub

    Private Sub btnProcess_Click(sender As Object, e As EventArgs) Handles btnProcess.Click
        If fileRead Then
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
            Dim addresses As String() = Split(e.Item.SubItems(2).Text, ",")
            cboTargetAddress.Items.Clear()
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
            updateIndirectJumpTarget()
        End If
    End Sub

    Private Sub btnRemoveTargetAddress_Click(sender As Object, e As EventArgs) Handles btnRemoveTargetAddress.Click
        If cboTargetAddress.Text <> "" Then
            cboTargetAddress.Items.Remove(cboTargetAddress.Text)
            updateIndirectJumpTarget()
        End If
    End Sub

    Private Sub updateIndirectJumpTarget()
        If lsvIndirectJmp.SelectedItems.Count = 0 Then Return
        Dim itx = lsvIndirectJmp.SelectedItems(0)
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
End Class
