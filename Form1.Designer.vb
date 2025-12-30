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
        txtMappingActivation = New TextBox()
        txtMappingSet = New TextBox()
        Label11 = New Label()
        Label10 = New Label()
        btnMappingActivationRemove = New Button()
        btnMappingActivationAdd = New Button()
        lsvMappingActivation = New ListView()
        ColumnHeader11 = New ColumnHeader()
        ColumnHeader13 = New ColumnHeader()
        Label9 = New Label()
        Label7 = New Label()
        btnRemoveMapping = New Button()
        btnAddMapping = New Button()
        cboMapToAddress = New ComboBox()
        lsvBanks = New ListView()
        ColumnHeader9 = New ColumnHeader()
        ColumnHeader10 = New ColumnHeader()
        ColumnHeader14 = New ColumnHeader()
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
        txtFilePath.Size = New Size(1160, 23)
        txtFilePath.TabIndex = 1
        ' 
        ' btnFilePicker
        ' 
        btnFilePicker.Anchor = AnchorStyles.Top Or AnchorStyles.Right
        btnFilePicker.Location = New Point(1233, 7)
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
        btnProcess.Location = New Point(1269, 7)
        btnProcess.Name = "btnProcess"
        btnProcess.Size = New Size(95, 81)
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
        TabControl1.Location = New Point(8, 94)
        TabControl1.Name = "TabControl1"
        TabControl1.SelectedIndex = 0
        TabControl1.Size = New Size(1356, 565)
        TabControl1.TabIndex = 7
        ' 
        ' TabPage1
        ' 
        TabPage1.Controls.Add(txtMappingActivation)
        TabPage1.Controls.Add(txtMappingSet)
        TabPage1.Controls.Add(Label11)
        TabPage1.Controls.Add(Label10)
        TabPage1.Controls.Add(btnMappingActivationRemove)
        TabPage1.Controls.Add(btnMappingActivationAdd)
        TabPage1.Controls.Add(lsvMappingActivation)
        TabPage1.Controls.Add(Label9)
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
        TabPage1.Size = New Size(1348, 537)
        TabPage1.TabIndex = 0
        TabPage1.Text = "Basic disassembly"
        TabPage1.UseVisualStyleBackColor = True
        ' 
        ' txtMappingActivation
        ' 
        txtMappingActivation.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        txtMappingActivation.Location = New Point(72, 440)
        txtMappingActivation.Name = "txtMappingActivation"
        txtMappingActivation.Size = New Size(248, 23)
        txtMappingActivation.TabIndex = 14
        ' 
        ' txtMappingSet
        ' 
        txtMappingSet.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        txtMappingSet.Location = New Point(72, 469)
        txtMappingSet.Name = "txtMappingSet"
        txtMappingSet.Size = New Size(248, 23)
        txtMappingSet.TabIndex = 15
        ' 
        ' Label11
        ' 
        Label11.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        Label11.AutoSize = True
        Label11.Location = New Point(9, 472)
        Label11.Name = "Label11"
        Label11.Size = New Size(25, 15)
        Label11.TabIndex = 30
        Label11.Text = "Set"
        ' 
        ' Label10
        ' 
        Label10.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        Label10.AutoSize = True
        Label10.Location = New Point(6, 443)
        Label10.Name = "Label10"
        Label10.Size = New Size(60, 15)
        Label10.TabIndex = 28
        Label10.Text = "Mapping"
        ' 
        ' btnMappingActivationRemove
        ' 
        btnMappingActivationRemove.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnMappingActivationRemove.Location = New Point(246, 499)
        btnMappingActivationRemove.Name = "btnMappingActivationRemove"
        btnMappingActivationRemove.Size = New Size(74, 27)
        btnMappingActivationRemove.TabIndex = 18
        btnMappingActivationRemove.Text = "Remove"
        btnMappingActivationRemove.UseVisualStyleBackColor = True
        ' 
        ' btnMappingActivationAdd
        ' 
        btnMappingActivationAdd.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnMappingActivationAdd.Location = New Point(166, 498)
        btnMappingActivationAdd.Name = "btnMappingActivationAdd"
        btnMappingActivationAdd.Size = New Size(74, 27)
        btnMappingActivationAdd.TabIndex = 17
        btnMappingActivationAdd.Text = "Add"
        btnMappingActivationAdd.UseVisualStyleBackColor = True
        ' 
        ' lsvMappingActivation
        ' 
        lsvMappingActivation.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        lsvMappingActivation.Columns.AddRange(New ColumnHeader() {ColumnHeader11, ColumnHeader13})
        lsvMappingActivation.Location = New Point(6, 230)
        lsvMappingActivation.Name = "lsvMappingActivation"
        lsvMappingActivation.Size = New Size(314, 204)
        lsvMappingActivation.TabIndex = 13
        lsvMappingActivation.UseCompatibleStateImageBehavior = False
        lsvMappingActivation.View = View.Details
        ' 
        ' ColumnHeader11
        ' 
        ColumnHeader11.Text = "Mapping"
        ColumnHeader11.Width = 200
        ' 
        ' ColumnHeader13
        ' 
        ColumnHeader13.Text = "Set"
        ColumnHeader13.Width = 80
        ' 
        ' Label9
        ' 
        Label9.AutoSize = True
        Label9.Location = New Point(6, 212)
        Label9.Name = "Label9"
        Label9.Size = New Size(118, 15)
        Label9.TabIndex = 24
        Label9.Text = "Mapping activation"
        ' 
        ' Label7
        ' 
        Label7.AutoSize = True
        Label7.Location = New Point(434, 8)
        Label7.Name = "Label7"
        Label7.Size = New Size(60, 15)
        Label7.TabIndex = 21
        Label7.Text = "Mapping"
        ' 
        ' btnRemoveMapping
        ' 
        btnRemoveMapping.Location = New Point(538, 172)
        btnRemoveMapping.Name = "btnRemoveMapping"
        btnRemoveMapping.Size = New Size(98, 27)
        btnRemoveMapping.TabIndex = 12
        btnRemoveMapping.Text = "Remove"
        btnRemoveMapping.UseVisualStyleBackColor = True
        ' 
        ' btnAddMapping
        ' 
        btnAddMapping.Location = New Point(434, 172)
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
        cboMapToAddress.Location = New Point(434, 26)
        cboMapToAddress.Name = "cboMapToAddress"
        cboMapToAddress.Size = New Size(202, 137)
        cboMapToAddress.TabIndex = 10
        ' 
        ' lsvBanks
        ' 
        lsvBanks.Columns.AddRange(New ColumnHeader() {ColumnHeader9, ColumnHeader10, ColumnHeader14})
        lsvBanks.FullRowSelect = True
        lsvBanks.Location = New Point(6, 26)
        lsvBanks.Name = "lsvBanks"
        lsvBanks.Size = New Size(422, 175)
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
        ' ColumnHeader14
        ' 
        ColumnHeader14.Text = "Bank ID"
        ' 
        ' Label6
        ' 
        Label6.AutoSize = True
        Label6.Location = New Point(6, 8)
        Label6.Name = "Label6"
        Label6.Size = New Size(131, 15)
        Label6.TabIndex = 16
        Label6.Text = "Switchable PRG banks"
        ' 
        ' Label5
        ' 
        Label5.AutoSize = True
        Label5.Location = New Point(326, 212)
        Label5.Name = "Label5"
        Label5.Size = New Size(86, 15)
        Label5.TabIndex = 15
        Label5.Text = "Indirect jumps"
        ' 
        ' btnRunIndirect
        ' 
        btnRunIndirect.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnRunIndirect.Location = New Point(324, 498)
        btnRunIndirect.Name = "btnRunIndirect"
        btnRunIndirect.Size = New Size(312, 27)
        btnRunIndirect.TabIndex = 17
        btnRunIndirect.Text = "Run indirect jump targets"
        btnRunIndirect.UseVisualStyleBackColor = True
        ' 
        ' btnRemoveTargetAddress
        ' 
        btnRemoveTargetAddress.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnRemoveTargetAddress.Location = New Point(538, 465)
        btnRemoveTargetAddress.Name = "btnRemoveTargetAddress"
        btnRemoveTargetAddress.Size = New Size(98, 27)
        btnRemoveTargetAddress.TabIndex = 16
        btnRemoveTargetAddress.Text = "Remove"
        btnRemoveTargetAddress.UseVisualStyleBackColor = True
        ' 
        ' btnAddTargetAddress
        ' 
        btnAddTargetAddress.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnAddTargetAddress.Location = New Point(538, 432)
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
        cboTargetAddress.Location = New Point(538, 230)
        cboTargetAddress.Name = "cboTargetAddress"
        cboTargetAddress.Size = New Size(98, 196)
        cboTargetAddress.TabIndex = 20
        ' 
        ' Label2
        ' 
        Label2.AutoSize = True
        Label2.Location = New Point(538, 212)
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
        lsvIndirectJmp.Location = New Point(326, 230)
        lsvIndirectJmp.Name = "lsvIndirectJmp"
        lsvIndirectJmp.Size = New Size(206, 262)
        lsvIndirectJmp.TabIndex = 19
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
        btnExportBasic.Location = New Point(1210, 496)
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
        lsvOutput.Location = New Point(642, 8)
        lsvOutput.Name = "lsvOutput"
        lsvOutput.Size = New Size(700, 484)
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
        TabPage2.Size = New Size(1348, 537)
        TabPage2.TabIndex = 1
        TabPage2.Text = "Analysis"
        TabPage2.UseVisualStyleBackColor = True
        ' 
        ' lblRemark
        ' 
        lblRemark.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        lblRemark.AutoSize = True
        lblRemark.Location = New Point(277, 594)
        lblRemark.Name = "lblRemark"
        lblRemark.Size = New Size(12, 15)
        lblRemark.TabIndex = 14
        lblRemark.Text = "/"
        ' 
        ' btnAnalyse
        ' 
        btnAnalyse.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnAnalyse.Location = New Point(196, 488)
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
        txtAnaCode4.Size = New Size(278, 514)
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
        txtAnaCode3.Size = New Size(278, 514)
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
        txtAnaCode2.Size = New Size(278, 514)
        txtAnaCode2.TabIndex = 10
        txtAnaCode2.WordWrap = False
        ' 
        ' btnLoadAna
        ' 
        btnLoadAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnLoadAna.Location = New Point(99, 488)
        btnLoadAna.Name = "btnLoadAna"
        btnLoadAna.Size = New Size(91, 33)
        btnLoadAna.TabIndex = 9
        btnLoadAna.Text = "Load File"
        btnLoadAna.UseVisualStyleBackColor = True
        ' 
        ' btnExportAna
        ' 
        btnExportAna.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        btnExportAna.Location = New Point(6, 488)
        btnExportAna.Name = "btnExportAna"
        btnExportAna.Size = New Size(87, 33)
        btnExportAna.TabIndex = 8
        btnExportAna.Text = "Export to file"
        btnExportAna.UseVisualStyleBackColor = True
        ' 
        ' txtAnaCode
        ' 
        txtAnaCode.Anchor = AnchorStyles.Top Or AnchorStyles.Bottom Or AnchorStyles.Left
        txtAnaCode.Location = New Point(6, 3)
        txtAnaCode.MaxLength = 1000000
        txtAnaCode.Multiline = True
        txtAnaCode.Name = "txtAnaCode"
        txtAnaCode.ReadOnly = True
        txtAnaCode.ScrollBars = ScrollBars.Both
        txtAnaCode.Size = New Size(278, 479)
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
        TabPage3.Size = New Size(1348, 537)
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
        txtCHeader.Size = New Size(343, 495)
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
        txtCCode.Size = New Size(1014, 495)
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
        txtIndirectAddress.Location = New Point(170, 36)
        txtIndirectAddress.Name = "txtIndirectAddress"
        txtIndirectAddress.Size = New Size(1093, 23)
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
        txtDataRange.Size = New Size(1093, 23)
        txtDataRange.TabIndex = 4
        ' 
        ' frmMain
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1370, 671)
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
    Friend WithEvents Label9 As Label
    Friend WithEvents Label10 As Label
    Friend WithEvents btnMappingActivationRemove As Button
    Friend WithEvents btnMappingActivationAdd As Button
    Friend WithEvents lsvMappingActivation As ListView
    Friend WithEvents txtMappingSet As TextBox
    Friend WithEvents Label11 As Label
    Friend WithEvents ColumnHeader11 As ColumnHeader
    Friend WithEvents ColumnHeader13 As ColumnHeader
    Friend WithEvents ColumnHeader14 As ColumnHeader
    Friend WithEvents txtMappingActivation As TextBox

End Class
