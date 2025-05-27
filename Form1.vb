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
End Class
