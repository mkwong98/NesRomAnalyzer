<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class frmMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Label1 = New Label()
        txtFilePath = New TextBox()
        btnFilePicker = New Button()
        ofdRomFile = New OpenFileDialog()
        btnProcess = New Button()
        TabControl1 = New TabControl()
        TabPage1 = New TabPage()
        btnExportBasic = New Button()
        lsvOutput = New ListView()
        ColumnHeader1 = New ColumnHeader()
        ColumnHeader2 = New ColumnHeader()
        ColumnHeader3 = New ColumnHeader()
        ColumnHeader4 = New ColumnHeader()
        ColumnHeader5 = New ColumnHeader()
        ColumnHeader6 = New ColumnHeader()
        ColumnHeader12 = New ColumnHeader()
        TabPage2 = New TabPage()
        lblRemark = New Label()
        btnAnalyse = New Button()
        txtAnaCode4 = New TextBox()
        txtAnaCode3 = New TextBox()
        txtAnaCode2 = New TextBox()
        btnLoadAna = New Button()
        btnExportAna = New Button()
        txtAnaCode = New TextBox()
        TabPage3 = New TabPage()
        txtCHeader = New TextBox()
        txtCCode = New TextBox()
        btnGenerate = New Button()
        sfdExportBasic = New SaveFileDialog()
        ofdAnaFile = New OpenFileDialog()
        sfdAnaFile = New SaveFileDialog()
        TabControl1.SuspendLayout()
        TabPage1.SuspendLayout()
        TabPage2.SuspendLayout()
        TabPage3.SuspendLayout()
        SuspendLayout()
        ' 
        ' Label1
        ' 
        Label1.AutoSize = True
        Label1.Location = New Point(8, 10)
        Label1.Name = "Label1"
        Label1.Size = New Size(60, 15)
        Label1.TabIndex = 0
        Label1.Text = "ROM file:"
        ' 
        ' txtFilePath
        ' 
        txtFilePath.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        txtFilePath.Location = New Point(74, 7)
        txtFilePath.Name = "txtFilePath"
        txtFilePath.Size = New Size(967, 23)
        txtFilePath.TabIndex = 1
        ' 
        ' btnFilePicker
        ' 
        btnFilePicker.Anchor = AnchorStyles.Top Or AnchorStyles.Right
        btnFilePicker.Location = New Point(1040, 7)
        btnFilePicker.Name = "btnFilePicker"
        btnFilePicker.Size = New Size(30, 24)
        btnFilePicker.TabIndex = 2
        btnFilePicker.Text = "..."
        btnFilePicker.UseVisualStyleBackColor = True
        ' 
        ' ofdRomFile
        ' 
        ofdRomFile.Filter = "ROM File|*.nes"
        ' 
        ' btnProcess
        ' 
        btnProcess.Anchor = AnchorStyles.Top Or AnchorStyles.Right
        btnProcess.Location = New Point(1076, 7)
        btnProcess.Name = "btnProcess"
        btnProcess.Size = New Size(95, 25)
        btnProcess.TabIndex = 3
        btnProcess.Text = "Process"
        btnProcess.UseVisualStyleBackColor = True
        ' 
        ' TabControl1
        ' 
        TabControl1.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        TabControl1.Controls.Add(TabPage1)
        TabControl1.Controls.Add(TabPage2)
        TabControl1.Controls.Add(TabPage3)
        TabControl1.Location = New Point(8, 38)
        TabControl1.Name = "TabControl1"
        TabControl1.SelectedIndex = 0
        TabControl1.Size = New Size(1163, 550)
        TabControl1.TabIndex = 6
        ' 
        ' TabPage1
        ' 
        TabPage1.Controls.Add(btnExportBasic)
        TabPage1.Controls.Add(lsvOutput)
        TabPage1.Location = New Point(4, 24)
        TabPage1.Name = "TabPage1"
        TabPage1.Padding = New Padding(3)
        TabPage1.Size = New Size(1155, 522)
        TabPage1.TabIndex = 0
        TabPage1.Text = "Basic disassembly"
        TabPage1.UseVisualStyleBackColor = True
        ' 
        ' btnExportBasic
        ' 
        btnExportBasic.Anchor = AnchorStyles.Bottom Or AnchorStyles.Right
        btnExportBasic.Location = New Point(1017, 481)
        btnExportBasic.Name = "btnExportBasic"
        btnExportBasic.Size = New Size(121, 33)
        btnExportBasic.TabIndex = 7
        btnExportBasic.Text = "Export results"
        btnExportBasic.UseVisualStyleBackColor = True
        ' 
        ' lsvOutput
        ' 
        lsvOutput.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        lsvOutput.Columns.AddRange(New ColumnHeader() {ColumnHeader1, ColumnHeader2, ColumnHeader3, ColumnHeader4, ColumnHeader5, ColumnHeader6, ColumnHeader12})
        lsvOutput.Location = New Point(6, 6)
        lsvOutput.Name = "lsvOutput"
        lsvOutput.Size = New Size(1132, 469)
        lsvOutput.TabIndex = 5
        lsvOutput.UseCompatibleStateImageBehavior = False
        lsvOutput.View = View.Details
        ' 
        ' ColumnHeader1
        ' 
        ColumnHeader1.Text = "Task name"
        ColumnHeader1.Width = 120
        ' 
        ' ColumnHeader2
        ' 
        ColumnHeader2.Text = "CPU address"
        ' 
        ' ColumnHeader3
        ' 
        ColumnHeader3.Text = "PRG ROM offset"
        ' 
        ' ColumnHeader4
        ' 
        ColumnHeader4.Text = "Bytes"
        ColumnHeader4.Width = 120
        ' 
        ' ColumnHeader5
        ' 
        ColumnHeader5.Text = "OP code"
        ' 
        ' ColumnHeader6
        ' 
        ColumnHeader6.Text = "Addressing mode"
        ColumnHeader6.Width = 120
        ' 
        ' ColumnHeader12
        ' 
        ColumnHeader12.Text = "Remarks"
        ColumnHeader12.Width = 180
        ' 
        ' TabPage2
        ' 
        TabPage2.Controls.Add(lblRemark)
        TabPage2.Controls.Add(btnAnalyse)
        TabPage2.Controls.Add(txtAnaCode4)
        TabPage2.Controls.Add(txtAnaCode3)
        TabPage2.Controls.Add(txtAnaCode2)
        TabPage2.Controls.Add(btnLoadAna)
        TabPage2.Controls.Add(btnExportAna)
        TabPage2.Controls.Add(txtAnaCode)
        TabPage2.Location = New Point(4, 24)
        TabPage2.Name = "TabPage2"
        TabPage2.Padding = New Padding(3)
        TabPage2.Size = New Size(1155, 522)
        TabPage2.TabIndex = 1
        TabPage2.Text = "Analysis"
        TabPage2.UseVisualStyleBackColor = True
        ' 
        ' lblRemark
        ' 
        lblRemark.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        lblRemark.AutoSize = True
        lblRemark.Location = New Point(277, 494)
        lblRemark.Name = "lblRemark"
        lblRemark.Size = New Size(12, 15)
        lblRemark.TabIndex = 14
        lblRemark.Text = "/"
        ' 
        ' btnAnalyse
        ' 
        btnAnalyse.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnAnalyse.Location = New Point(196, 483)
        btnAnalyse.Name = "btnAnalyse"
        btnAnalyse.Size = New Size(75, 33)
        btnAnalyse.TabIndex = 13
        btnAnalyse.Text = "Analyse"
        btnAnalyse.UseVisualStyleBackColor = True
        ' 
        ' txtAnaCode4
        ' 
        txtAnaCode4.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtAnaCode4.Location = New Point(858, 7)
        txtAnaCode4.MaxLength = 1000000
        txtAnaCode4.Multiline = True
        txtAnaCode4.Name = "txtAnaCode4"
        txtAnaCode4.ReadOnly = True
        txtAnaCode4.ScrollBars = ScrollBars.Both
        txtAnaCode4.Size = New Size(278, 470)
        txtAnaCode4.TabIndex = 12
        txtAnaCode4.WordWrap = False
        ' 
        ' txtAnaCode3
        ' 
        txtAnaCode3.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtAnaCode3.Location = New Point(574, 7)
        txtAnaCode3.MaxLength = 1000000
        txtAnaCode3.Multiline = True
        txtAnaCode3.Name = "txtAnaCode3"
        txtAnaCode3.ReadOnly = True
        txtAnaCode3.ScrollBars = ScrollBars.Both
        txtAnaCode3.Size = New Size(278, 470)
        txtAnaCode3.TabIndex = 11
        txtAnaCode3.WordWrap = False
        ' 
        ' txtAnaCode2
        ' 
        txtAnaCode2.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtAnaCode2.Location = New Point(290, 7)
        txtAnaCode2.MaxLength = 1000000
        txtAnaCode2.Multiline = True
        txtAnaCode2.Name = "txtAnaCode2"
        txtAnaCode2.ReadOnly = True
        txtAnaCode2.ScrollBars = ScrollBars.Both
        txtAnaCode2.Size = New Size(278, 470)
        txtAnaCode2.TabIndex = 10
        txtAnaCode2.WordWrap = False
        ' 
        ' btnLoadAna
        ' 
        btnLoadAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnLoadAna.Location = New Point(99, 483)
        btnLoadAna.Name = "btnLoadAna"
        btnLoadAna.Size = New Size(91, 33)
        btnLoadAna.TabIndex = 9
        btnLoadAna.Text = "Load File"
        btnLoadAna.UseVisualStyleBackColor = True
        ' 
        ' btnExportAna
        ' 
        btnExportAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnExportAna.Location = New Point(6, 483)
        btnExportAna.Name = "btnExportAna"
        btnExportAna.Size = New Size(87, 33)
        btnExportAna.TabIndex = 8
        btnExportAna.Text = "Export to file"
        btnExportAna.UseVisualStyleBackColor = True
        ' 
        ' txtAnaCode
        ' 
        txtAnaCode.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtAnaCode.Location = New Point(6, 7)
        txtAnaCode.MaxLength = 1000000
        txtAnaCode.Multiline = True
        txtAnaCode.Name = "txtAnaCode"
        txtAnaCode.ReadOnly = True
        txtAnaCode.ScrollBars = ScrollBars.Both
        txtAnaCode.Size = New Size(278, 470)
        txtAnaCode.TabIndex = 0
        txtAnaCode.WordWrap = False
        ' 
        ' TabPage3
        ' 
        TabPage3.Controls.Add(txtCHeader)
        TabPage3.Controls.Add(txtCCode)
        TabPage3.Controls.Add(btnGenerate)
        TabPage3.Location = New Point(4, 24)
        TabPage3.Name = "TabPage3"
        TabPage3.Size = New Size(1155, 522)
        TabPage3.TabIndex = 2
        TabPage3.Text = "Code generation"
        TabPage3.UseVisualStyleBackColor = True
        ' 
        ' txtCHeader
        ' 
        txtCHeader.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtCHeader.Location = New Point(3, 39)
        txtCHeader.Multiline = True
        txtCHeader.Name = "txtCHeader"
        txtCHeader.ReadOnly = True
        txtCHeader.ScrollBars = ScrollBars.Both
        txtCHeader.Size = New Size(343, 473)
        txtCHeader.TabIndex = 2
        ' 
        ' txtCCode
        ' 
        txtCCode.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        txtCCode.Location = New Point(352, 39)
        txtCCode.Multiline = True
        txtCCode.Name = "txtCCode"
        txtCCode.ReadOnly = True
        txtCCode.ScrollBars = ScrollBars.Both
        txtCCode.Size = New Size(785, 473)
        txtCCode.TabIndex = 1
        ' 
        ' btnGenerate
        ' 
        btnGenerate.Location = New Point(9, 6)
        btnGenerate.Name = "btnGenerate"
        btnGenerate.Size = New Size(82, 27)
        btnGenerate.TabIndex = 0
        btnGenerate.Text = "Generate"
        btnGenerate.UseVisualStyleBackColor = True
        ' 
        ' sfdExportBasic
        ' 
        sfdExportBasic.DefaultExt = "csv"
        sfdExportBasic.Filter = "CSV File|*.csv"
        ' 
        ' ofdAnaFile
        ' 
        ofdAnaFile.Filter = "CSV file|*.csv"
        ' 
        ' sfdAnaFile
        ' 
        sfdAnaFile.DefaultExt = "csv"
        sfdAnaFile.Filter = "CSV File|*.csv"
        ' 
        ' frmMain
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1177, 600)
        Controls.Add(TabControl1)
        Controls.Add(btnProcess)
        Controls.Add(btnFilePicker)
        Controls.Add(txtFilePath)
        Controls.Add(Label1)
        Name = "frmMain"
        Text = "Main"
        TabControl1.ResumeLayout(False)
        TabPage1.ResumeLayout(False)
        TabPage2.ResumeLayout(False)
        TabPage2.PerformLayout()
        TabPage3.ResumeLayout(False)
        TabPage3.PerformLayout()
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents txtFilePath As TextBox
    Friend WithEvents btnFilePicker As Button
    Friend WithEvents ofdRomFile As OpenFileDialog
    Friend WithEvents btnProcess As Button
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents lsvOutput As ListView
    Friend WithEvents ColumnHeader1 As ColumnHeader
    Friend WithEvents ColumnHeader2 As ColumnHeader
    Friend WithEvents ColumnHeader3 As ColumnHeader
    Friend WithEvents ColumnHeader4 As ColumnHeader
    Friend WithEvents ColumnHeader5 As ColumnHeader
    Friend WithEvents ColumnHeader6 As ColumnHeader
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents btnExportBasic As Button
    Friend WithEvents sfdExportBasic As SaveFileDialog
    Friend WithEvents ColumnHeader12 As ColumnHeader
    Friend WithEvents txtAnaCode As TextBox
    Friend WithEvents btnExportAna As Button
    Friend WithEvents btnLoadAna As Button
    Friend WithEvents ofdAnaFile As OpenFileDialog
    Friend WithEvents sfdAnaFile As SaveFileDialog
    Friend WithEvents txtAnaCode4 As TextBox
    Friend WithEvents txtAnaCode3 As TextBox
    Friend WithEvents txtAnaCode2 As TextBox
    Friend WithEvents btnAnalyse As Button
    Friend WithEvents lblRemark As Label
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents txtCCode As TextBox
    Friend WithEvents btnGenerate As Button
    Friend WithEvents txtCHeader As TextBox

End Class
