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
        Label8 = New Label()
        lstModes = New ListBox()
        Label7 = New Label()
        btnRemoveMapping = New Button()
        btnAddMapping = New Button()
        cboMapToAddress = New ComboBox()
        lsvBanks = New ListView()
        ColumnHeader9 = New ColumnHeader()
        ColumnHeader10 = New ColumnHeader()
        Label6 = New Label()
        Label5 = New Label()
        btnRunIndirect = New Button()
        btnRemoveTargetAddress = New Button()
        btnAddTargetAddress = New Button()
        cboTargetAddress = New ComboBox()
        Label2 = New Label()
        lsvIndirectJmp = New ListView()
        ColumnHeader7 = New ColumnHeader()
        ColumnHeader8 = New ColumnHeader()
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
        Label3 = New Label()
        txtIndirectAddress = New TextBox()
        Label4 = New Label()
        txtDataRange = New TextBox()
        Label9 = New Label()
        txtSwitchBankPoints = New TextBox()
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
        btnProcess.Size = New Size(95, 111)
        btnProcess.TabIndex = 6
        btnProcess.Text = "Process"
        btnProcess.UseVisualStyleBackColor = True
        ' 
        ' TabControl1
        ' 
        TabControl1.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        TabControl1.Controls.Add(TabPage1)
        TabControl1.Controls.Add(TabPage2)
        TabControl1.Controls.Add(TabPage3)
        TabControl1.Location = New Point(8, 121)
        TabControl1.Name = "TabControl1"
        TabControl1.SelectedIndex = 0
        TabControl1.Size = New Size(1163, 458)
        TabControl1.TabIndex = 7
        ' 
        ' TabPage1
        ' 
        TabPage1.Controls.Add(Label8)
        TabPage1.Controls.Add(lstModes)
        TabPage1.Controls.Add(Label7)
        TabPage1.Controls.Add(btnRemoveMapping)
        TabPage1.Controls.Add(btnAddMapping)
        TabPage1.Controls.Add(cboMapToAddress)
        TabPage1.Controls.Add(lsvBanks)
        TabPage1.Controls.Add(Label6)
        TabPage1.Controls.Add(Label5)
        TabPage1.Controls.Add(btnRunIndirect)
        TabPage1.Controls.Add(btnRemoveTargetAddress)
        TabPage1.Controls.Add(btnAddTargetAddress)
        TabPage1.Controls.Add(cboTargetAddress)
        TabPage1.Controls.Add(Label2)
        TabPage1.Controls.Add(lsvIndirectJmp)
        TabPage1.Controls.Add(btnExportBasic)
        TabPage1.Controls.Add(lsvOutput)
        TabPage1.Location = New Point(4, 24)
        TabPage1.Name = "TabPage1"
        TabPage1.Padding = New Padding(3)
        TabPage1.Size = New Size(1155, 430)
        TabPage1.TabIndex = 0
        TabPage1.Text = "Basic disassembly"
        TabPage1.UseVisualStyleBackColor = True
        ' 
        ' Label8
        ' 
        Label8.Location = New Point(6, 8)
        Label8.Name = "Label8"
        Label8.Size = New Size(67, 60)
        Label8.TabIndex = 23
        Label8.Text = "Switching mode"
        ' 
        ' lstModes
        ' 
        lstModes.FormattingEnabled = True
        lstModes.ItemHeight = 15
        lstModes.Location = New Point(79, 6)
        lstModes.Name = "lstModes"
        lstModes.SelectionMode = SelectionMode.MultiExtended
        lstModes.Size = New Size(237, 79)
        lstModes.TabIndex = 8
        ' 
        ' Label7
        ' 
        Label7.AutoSize = True
        Label7.Location = New Point(218, 88)
        Label7.Name = "Label7"
        Label7.Size = New Size(60, 15)
        Label7.TabIndex = 21
        Label7.Text = "Mapping"
        ' 
        ' btnRemoveMapping
        ' 
        btnRemoveMapping.Location = New Point(218, 222)
        btnRemoveMapping.Name = "btnRemoveMapping"
        btnRemoveMapping.Size = New Size(98, 27)
        btnRemoveMapping.TabIndex = 12
        btnRemoveMapping.Text = "Remove"
        btnRemoveMapping.UseVisualStyleBackColor = True
        ' 
        ' btnAddMapping
        ' 
        btnAddMapping.Location = New Point(218, 189)
        btnAddMapping.Name = "btnAddMapping"
        btnAddMapping.Size = New Size(98, 27)
        btnAddMapping.TabIndex = 11
        btnAddMapping.Text = "Add"
        btnAddMapping.UseVisualStyleBackColor = True
        ' 
        ' cboMapToAddress
        ' 
        cboMapToAddress.DropDownStyle = ComboBoxStyle.Simple
        cboMapToAddress.FormattingEnabled = True
        cboMapToAddress.Location = New Point(218, 106)
        cboMapToAddress.Name = "cboMapToAddress"
        cboMapToAddress.Size = New Size(98, 77)
        cboMapToAddress.TabIndex = 10
        ' 
        ' lsvBanks
        ' 
        lsvBanks.Columns.AddRange(New ColumnHeader() {ColumnHeader9, ColumnHeader10})
        lsvBanks.FullRowSelect = True
        lsvBanks.Location = New Point(6, 106)
        lsvBanks.Name = "lsvBanks"
        lsvBanks.Size = New Size(206, 143)
        lsvBanks.TabIndex = 9
        lsvBanks.UseCompatibleStateImageBehavior = False
        lsvBanks.View = View.Details
        ' 
        ' ColumnHeader9
        ' 
        ColumnHeader9.Text = "Bank name"
        ColumnHeader9.Width = 100
        ' 
        ' ColumnHeader10
        ' 
        ColumnHeader10.Text = "Bank size (KB)"
        ColumnHeader10.Width = 100
        ' 
        ' Label6
        ' 
        Label6.AutoSize = True
        Label6.Location = New Point(6, 88)
        Label6.Name = "Label6"
        Label6.Size = New Size(131, 15)
        Label6.TabIndex = 16
        Label6.Text = "Switchable PRG banks"
        ' 
        ' Label5
        ' 
        Label5.AutoSize = True
        Label5.Location = New Point(6, 261)
        Label5.Name = "Label5"
        Label5.Size = New Size(86, 15)
        Label5.TabIndex = 15
        Label5.Text = "Indirect jumps"
        ' 
        ' btnRunIndirect
        ' 
        btnRunIndirect.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnRunIndirect.Location = New Point(4, 395)
        btnRunIndirect.Name = "btnRunIndirect"
        btnRunIndirect.Size = New Size(312, 27)
        btnRunIndirect.TabIndex = 17
        btnRunIndirect.Text = "Run indirect jump targets"
        btnRunIndirect.UseVisualStyleBackColor = True
        ' 
        ' btnRemoveTargetAddress
        ' 
        btnRemoveTargetAddress.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnRemoveTargetAddress.Location = New Point(218, 362)
        btnRemoveTargetAddress.Name = "btnRemoveTargetAddress"
        btnRemoveTargetAddress.Size = New Size(98, 27)
        btnRemoveTargetAddress.TabIndex = 16
        btnRemoveTargetAddress.Text = "Remove"
        btnRemoveTargetAddress.UseVisualStyleBackColor = True
        ' 
        ' btnAddTargetAddress
        ' 
        btnAddTargetAddress.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnAddTargetAddress.Location = New Point(218, 329)
        btnAddTargetAddress.Name = "btnAddTargetAddress"
        btnAddTargetAddress.Size = New Size(98, 27)
        btnAddTargetAddress.TabIndex = 15
        btnAddTargetAddress.Text = "Add"
        btnAddTargetAddress.UseVisualStyleBackColor = True
        ' 
        ' cboTargetAddress
        ' 
        cboTargetAddress.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        cboTargetAddress.DropDownStyle = ComboBoxStyle.Simple
        cboTargetAddress.FormattingEnabled = True
        cboTargetAddress.Location = New Point(218, 279)
        cboTargetAddress.Name = "cboTargetAddress"
        cboTargetAddress.Size = New Size(98, 44)
        cboTargetAddress.TabIndex = 14
        ' 
        ' Label2
        ' 
        Label2.AutoSize = True
        Label2.Location = New Point(218, 263)
        Label2.Name = "Label2"
        Label2.Size = New Size(103, 15)
        Label2.TabIndex = 10
        Label2.Text = "Target addresses"
        ' 
        ' lsvIndirectJmp
        ' 
        lsvIndirectJmp.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        lsvIndirectJmp.Columns.AddRange(New ColumnHeader() {ColumnHeader7, ColumnHeader8})
        lsvIndirectJmp.FullRowSelect = True
        lsvIndirectJmp.Location = New Point(6, 279)
        lsvIndirectJmp.Name = "lsvIndirectJmp"
        lsvIndirectJmp.Size = New Size(206, 110)
        lsvIndirectJmp.TabIndex = 13
        lsvIndirectJmp.UseCompatibleStateImageBehavior = False
        lsvIndirectJmp.View = View.Details
        ' 
        ' ColumnHeader7
        ' 
        ColumnHeader7.Text = "PRG ROM Address"
        ColumnHeader7.Width = 100
        ' 
        ' ColumnHeader8
        ' 
        ColumnHeader8.Text = "RAM location"
        ColumnHeader8.Width = 100
        ' 
        ' btnExportBasic
        ' 
        btnExportBasic.Anchor = AnchorStyles.Bottom Or AnchorStyles.Right
        btnExportBasic.Location = New Point(1017, 389)
        btnExportBasic.Name = "btnExportBasic"
        btnExportBasic.Size = New Size(121, 33)
        btnExportBasic.TabIndex = 19
        btnExportBasic.Text = "Export results"
        btnExportBasic.UseVisualStyleBackColor = True
        ' 
        ' lsvOutput
        ' 
        lsvOutput.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left Or AnchorStyles.Right
        lsvOutput.Columns.AddRange(New ColumnHeader() {ColumnHeader1, ColumnHeader2, ColumnHeader3, ColumnHeader4, ColumnHeader5, ColumnHeader6, ColumnHeader12})
        lsvOutput.Location = New Point(327, 8)
        lsvOutput.Name = "lsvOutput"
        lsvOutput.Size = New Size(822, 377)
        lsvOutput.TabIndex = 18
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
        TabPage2.Size = New Size(1155, 430)
        TabPage2.TabIndex = 1
        TabPage2.Text = "Analysis"
        TabPage2.UseVisualStyleBackColor = True
        ' 
        ' lblRemark
        ' 
        lblRemark.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        lblRemark.AutoSize = True
        lblRemark.Location = New Point(277, 485)
        lblRemark.Name = "lblRemark"
        lblRemark.Size = New Size(12, 15)
        lblRemark.TabIndex = 14
        lblRemark.Text = "/"
        ' 
        ' btnAnalyse
        ' 
        btnAnalyse.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnAnalyse.Location = New Point(196, 391)
        btnAnalyse.Name = "btnAnalyse"
        btnAnalyse.Size = New Size(88, 33)
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
        txtAnaCode4.Size = New Size(278, 417)
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
        txtAnaCode3.Size = New Size(278, 417)
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
        txtAnaCode2.Size = New Size(278, 417)
        txtAnaCode2.TabIndex = 10
        txtAnaCode2.WordWrap = False
        ' 
        ' btnLoadAna
        ' 
        btnLoadAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnLoadAna.Location = New Point(99, 391)
        btnLoadAna.Name = "btnLoadAna"
        btnLoadAna.Size = New Size(91, 33)
        btnLoadAna.TabIndex = 9
        btnLoadAna.Text = "Load File"
        btnLoadAna.UseVisualStyleBackColor = True
        ' 
        ' btnExportAna
        ' 
        btnExportAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnExportAna.Location = New Point(6, 391)
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
        txtAnaCode.Size = New Size(278, 378)
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
        TabPage3.Size = New Size(1155, 430)
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
        txtCHeader.Size = New Size(343, 388)
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
        txtCCode.Size = New Size(800, 388)
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
        ' Label3
        ' 
        Label3.AutoSize = True
        Label3.Location = New Point(8, 39)
        Label3.Name = "Label3"
        Label3.Size = New Size(140, 15)
        Label3.TabIndex = 7
        Label3.Text = "Indirect jump addresses"
        ' 
        ' txtIndirectAddress
        ' 
        txtIndirectAddress.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        txtIndirectAddress.Location = New Point(154, 36)
        txtIndirectAddress.Name = "txtIndirectAddress"
        txtIndirectAddress.Size = New Size(916, 23)
        txtIndirectAddress.TabIndex = 3
        ' 
        ' Label4
        ' 
        Label4.AutoSize = True
        Label4.Location = New Point(8, 68)
        Label4.Name = "Label4"
        Label4.Size = New Size(156, 15)
        Label4.TabIndex = 8
        Label4.Text = "Data block address ranges"
        ' 
        ' txtDataRange
        ' 
        txtDataRange.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        txtDataRange.Location = New Point(170, 65)
        txtDataRange.Name = "txtDataRange"
        txtDataRange.Size = New Size(900, 23)
        txtDataRange.TabIndex = 4
        ' 
        ' Label9
        ' 
        Label9.AutoSize = True
        Label9.Location = New Point(12, 98)
        Label9.Name = "Label9"
        Label9.Size = New Size(112, 15)
        Label9.TabIndex = 10
        Label9.Text = "Switch bank points"
        ' 
        ' txtSwitchBankPoints
        ' 
        txtSwitchBankPoints.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right
        txtSwitchBankPoints.Location = New Point(130, 95)
        txtSwitchBankPoints.Name = "txtSwitchBankPoints"
        txtSwitchBankPoints.Size = New Size(940, 23)
        txtSwitchBankPoints.TabIndex = 5
        ' 
        ' frmMain
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1177, 591)
        Controls.Add(txtSwitchBankPoints)
        Controls.Add(Label9)
        Controls.Add(txtDataRange)
        Controls.Add(Label4)
        Controls.Add(txtIndirectAddress)
        Controls.Add(Label3)
        Controls.Add(TabControl1)
        Controls.Add(btnProcess)
        Controls.Add(btnFilePicker)
        Controls.Add(txtFilePath)
        Controls.Add(Label1)
        Name = "frmMain"
        Text = "Main"
        TabControl1.ResumeLayout(False)
        TabPage1.ResumeLayout(False)
        TabPage1.PerformLayout()
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
    Friend WithEvents lsvIndirectJmp As ListView
    Friend WithEvents ColumnHeader7 As ColumnHeader
    Friend WithEvents ColumnHeader8 As ColumnHeader
    Friend WithEvents Label2 As Label
    Friend WithEvents btnAddTargetAddress As Button
    Friend WithEvents cboTargetAddress As ComboBox
    Friend WithEvents btnRemoveTargetAddress As Button
    Friend WithEvents btnRunIndirect As Button
    Friend WithEvents Label3 As Label
    Friend WithEvents txtIndirectAddress As TextBox
    Friend WithEvents Label4 As Label
    Friend WithEvents txtDataRange As TextBox
    Friend WithEvents Label5 As Label
    Friend WithEvents lsvBanks As ListView
    Friend WithEvents ColumnHeader9 As ColumnHeader
    Friend WithEvents ColumnHeader10 As ColumnHeader
    Friend WithEvents Label6 As Label
    Friend WithEvents btnRemoveMapping As Button
    Friend WithEvents btnAddMapping As Button
    Friend WithEvents cboMapToAddress As ComboBox
    Friend WithEvents Label7 As Label
    Friend WithEvents Label8 As Label
    Friend WithEvents lstModes As ListBox
    Friend WithEvents Label9 As Label
    Friend WithEvents txtSwitchBankPoints As TextBox

End Class
