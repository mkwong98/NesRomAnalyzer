Imports System.CodeDom
Imports System.Collections.Specialized.BitVector32
Imports System.Net
Imports System.Runtime.InteropServices
Imports System.Text.Json
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.TrackBar
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.TreeView
Imports Microsoft.VisualBasic.Devices

Public Enum TaskType
    RESET
    NMI
    BRK
    JSR
End Enum

Structure taskToRun
    Public id As Integer
    Public type As TaskType
    Public address As UInt16
    Public realAddress As UInt32
    Public mapperConfig As UInt64
    Public name As String
End Structure

Public Structure addressRange
    Public rangeStart As UInt32
    Public rangeEnd As UInt32
End Structure

Public Structure structRange
    Public rangeStart As UInt32
    Public rangeEnd As UInt32
    Public nextAddress As UInt32
    Public type As BlockType

    Public hasElse As Boolean
    Public elseRangeStart As UInt32
    Public elseRangeEnd As UInt32
    Public elseNextAddress As UInt32
    Public combinedEnd As UInt32
End Structure

Public Structure stackEntry
    Public source As memoryID
    Public name As String
End Structure

Class InstComparer
    Implements IComparer(Of instruction)
    Public Function Compare(x As instruction, y As instruction) As Integer Implements IComparer(Of instruction).Compare
        If x.realAddress < y.realAddress Then
            Return -1
        ElseIf x.realAddress = y.realAddress Then
            Return 0
        Else
            Return 1
        End If
    End Function
End Class

Class addressRangeDestComparer
    Implements IComparer(Of addressRange)
    Public Function Compare(x As addressRange, y As addressRange) As Integer Implements IComparer(Of addressRange).Compare
        If x.rangeEnd < y.rangeEnd Then
            Return -1
        ElseIf x.rangeEnd = y.rangeEnd Then
            Return 0
        Else
            Return 1
        End If
    End Function
End Class

Module analyzer
    Public frm As frmMain
    Private tasksToRun As New List(Of taskToRun)
    Private lines As New List(Of block)
    Private currentTask As Integer
    Private currentBlock As block

    Private fullCode As New List(Of instruction)
    Private nmiAddress As UInt32
    Private resetAddress As UInt32
    Private brkAddress As UInt32
    Private hasBrk As Boolean = False
    Private brkTraced As Boolean
    Private traceTasksToRun As New List(Of traceTask)
    Private jumpLinks As New List(Of addressRange)
    Private tmpTracedAddress As New List(Of UInt32)
    Private codeSections As New List(Of addressRange)
    Private blocks As New List(Of block)



    Public Sub start()

        console.init()

        currentTask = 0

        'add first 2 tasks
        Dim t As taskToRun
        Dim m As memoryID
        m.Type = MemoryType.INIT
        m.ID = 0
        t = initTaskForInterrupt(&HFFFC, m)
        t.id = 0
        t.type = TaskType.RESET
        t.name = "RESET"
        tasksToRun.Add(t)

        t = initTaskForInterrupt(&HFFFA, m)
        t.id = 1
        t.type = TaskType.NMI
        t.name = "NMI"
        tasksToRun.Add(t)

        Dim i As ListViewItem
        While currentTask < tasksToRun.Count
            'set up
            t = tasksToRun(currentTask)
            console.setUpForTask(t)
            i = frm.lsvOutput.Items.Add("")
            currentBlock = New block
            currentBlock.name = t.name
            lines.Add(currentBlock)
            Select Case t.type
                Case TaskType.RESET
                    i.Text = "RES"
                    currentBlock.type = BlockType.RESET
                    resetAddress = t.realAddress
                Case TaskType.NMI
                    i.Text = "NMI"
                    currentBlock.type = BlockType.NMI
                    nmiAddress = t.realAddress
                Case TaskType.BRK
                    i.Text = "BRK"
                    currentBlock.type = BlockType.BRK
                    brkAddress = t.realAddress
                    hasBrk = True
                Case TaskType.JSR
                    i.Text = "SUB"
                    currentBlock.type = BlockType.SUBROUNTINE
            End Select
            i.Text = i.Text & " " & Hex(t.realAddress)

            tasksToRun(currentTask) = t
            'start running
            console.run()
            i = frm.lsvOutput.Items.Add("END")
            tasksToRun(currentTask) = t
            currentTask += 1
            Application.DoEvents()
        End While

        frmMain.txtAnaCode.Text = ""
        For Each b As block In lines
            If b.code.Count > 0 Then
                Dim s As String = b.saveToString()
                If s <> "" Then
                    frmMain.txtAnaCode.Text &= s
                End If
            End If
        Next

        MsgBox("Basic steps completed")

    End Sub

    Public Sub addJSRTask(pAddress As UInt16, pRealAddress As UInt32)
        Dim t As taskToRun
        t.id = tasksToRun.Count
        t.type = TaskType.JSR
        t.address = pAddress
        t.realAddress = pRealAddress
        t.mapperConfig = getMapperConfig()
        t.name = "SUB_" & t.realAddress.ToString("X6")
        If Not taskIsDuplicate(t) Then
            tasksToRun.Add(t)
        End If
    End Sub

    Public Sub addBRKTask(pSource As memoryID)
        Dim t As taskToRun = initTaskForInterrupt(&HFFFE, pSource)
        t.type = TaskType.BRK
        t.name = "BRK"
        If Not taskIsDuplicate(t) Then
            tasksToRun.Add(t)
        End If
    End Sub

    Public Function initTaskForInterrupt(pAddress As UInt16, pSource As memoryID) As taskToRun
        Dim t As taskToRun
        t.id = tasksToRun.Count
        t.address = readAsAddress(pAddress, PrgByteType.INTERRUPT_VECTOR, pSource.ID)
        t.realAddress = read(t.address, PrgByteType.PEEK, 0).source.ID
        t.mapperConfig = getMapperConfig()
        Return t
    End Function

    Public Function taskIsDuplicate(task As taskToRun) As Boolean
        For Each t As taskToRun In tasksToRun
            If t.realAddress = task.realAddress Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function traceTaskIsDuplicate(task As traceTask) As Boolean
        For Each t As traceTask In traceTasksToRun
            If t.realAddress = task.realAddress Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Sub logLineOfCode(pAddress As UInt16, pRealAddress As UInt32, pOpCode As Byte, pOp1 As Byte, pOp2 As Byte, pRemarks As String, pInst As instruction)
        currentBlock.code.Add(pInst)
        If currentBlock.code.Count = 1 Then
            currentBlock.realAddress = pRealAddress
        End If

        Dim i As ListViewItem = frm.lsvOutput.Items.Add("")
        i.SubItems.Add(addressToHexStr(pAddress))
        i.SubItems.Add(realAddressToHexStr(pRealAddress))

        i.SubItems.Add(byteToHexStr(pOpCode) & IIf(opTable(pOpCode).length > 1, " " & byteToHexStr(pOp1), "") & IIf(opTable(pOpCode).length > 2, " " & byteToHexStr(pOp2), ""))
        i.SubItems.Add(opTable(pOpCode).name)
        Select Case opTable(pOpCode).mode
            Case AddressingMode.IMPLICIT
                i.SubItems.Add("IMPLICIT")
            Case AddressingMode.ACCUMULATOR
                i.SubItems.Add("ACCUMULATOR")
            Case AddressingMode.IMMEDIATE
                i.SubItems.Add("IMMEDIATE")
            Case AddressingMode.ZERO_PAGE
                i.SubItems.Add("ZERO_PAGE")
            Case AddressingMode.ABSOLUTE
                i.SubItems.Add("ABSOLUTE")
            Case AddressingMode.RELATIVE
                i.SubItems.Add("RELATIVE")
            Case AddressingMode.INDIRECT
                i.SubItems.Add("INDIRECT")
            Case AddressingMode.ZERO_PAGE_INDEXED_X
                i.SubItems.Add("ZERO_PAGE_INDEXED_X")
            Case AddressingMode.ZERO_PAGE_INDEXED_Y
                i.SubItems.Add("ZERO_PAGE_INDEXED_Y")
            Case AddressingMode.ABSOLUTE_INDEXED_X
                i.SubItems.Add("ABSOLUTE_INDEXED_X")
            Case AddressingMode.ABSOLUTE_INDEXED_Y
                i.SubItems.Add("ABSOLUTE_INDEXED_Y")
            Case AddressingMode.INDEXED_INDIRECT_X
                i.SubItems.Add("INDEXED_INDIRECT_X")
            Case AddressingMode.INDIRECT_INDEXED_Y
                i.SubItems.Add("INDIRECT_INDEXED_Y")
        End Select
        i.SubItems.Add(pRemarks)
        Application.DoEvents()
    End Sub

    Public Sub logBranch(pAddress As UInt16, pRealAddress As UInt32)
        Dim i As ListViewItem = frm.lsvOutput.Items.Add("")
        i.SubItems.Add("BRANCH")
        i.SubItems.Add(realAddressToHexStr(pRealAddress))

    End Sub

    Public Sub analyse()
        'put all instructions together and sort by real address
        Dim fullBlock As New block
        For Each b As block In lines
            fullBlock.takeAllInstructions(b)
        Next
        fullCode.Clear()
        For Each b As codeBlock In fullBlock.code
            Dim i As instruction = CType(b, instruction)
            fullCode.Add(i)
        Next
        If fullCode.Count = 0 Then
            MsgBox("Error: No code found")
            Return
        End If

        fullCode.Sort(New InstComparer)
        jumpLinks.Clear()

        'trace the code again
        For Each i As instruction In fullCode
            i.traceMarking = ""
            Select Case i.type
                Case InstructionType.BRANCH
                    Dim b As instBranch = CType(i, instBranch)
                    Dim r As addressRange
                    r.rangeStart = b.realAddress
                    r.rangeEnd = b.branchToAddress
                    jumpLinks.Add(r)
                Case InstructionType.JUMP
                    Dim b As instJump = CType(i, instJump)
                    Dim r As addressRange
                    r.rangeStart = b.realAddress
                    If b.isIndirect Then
                        'not sure what to do yet
                    Else
                        r.rangeEnd = b.jumpToRealAddress
                        jumpLinks.Add(r)
                    End If
                Case InstructionType.SUBROUTINE
                    Dim b As instSubroutine = CType(i, instSubroutine)
                    Dim r As addressRange
                    r.rangeStart = b.realAddress
                    r.rangeEnd = b.subRealAddress
                    jumpLinks.Add(r)
            End Select
        Next
        brkTraced = False
        frm.txtAnaCode3.Text = ""
        jumpLinks.Sort(New addressRangeDestComparer)
        For Each j As addressRange In jumpLinks
            Dim p As Integer = findInstructionIndex(j.rangeEnd, 0, fullCode.Count - 1)
            If p <> -1 Then
                Dim tInst As instruction = fullCode(p)
                tInst.isJumpTarget = True
            End If
        Next

        traceTasksToRun.Clear()
        Dim t As New traceTask
        t.name = "Reset"
        t.realAddress = resetAddress
        t.type = TaskType.RESET
        t.source = UInt32.MaxValue
        traceTasksToRun.Add(t)
        traceBranch(t)

        t = New traceTask
        t.name = "NMI"
        t.realAddress = nmiAddress
        t.type = TaskType.NMI
        t.source = UInt32.MaxValue
        traceTasksToRun.Add(t)
        traceBranch(t)

        If hasBrk And Not brkTraced Then
            t = New traceTask
            t.name = "BRK"
            t.realAddress = brkAddress
            t.type = TaskType.BRK
            t.source = UInt32.MaxValue
            traceTasksToRun.Add(t)
            traceBranch(t)
        End If


        'trace all required reg and flag changes
        For i As Integer = 1 To fullCode.Count - 1
            frm.lblRemark.Text = i & " / " & fullCode.Count

            Dim flgC, flgZ, flgI, flgD, flgV, flgN As Boolean
            fullCode(i).getRequiredFlags(flgC, flgZ, flgI, flgD, flgV, flgN)
            Dim regReqirements As List(Of memoryTarget) = fullCode(i).getRequiredMemoryTarget()
            Dim regA As Boolean = False
            Dim regX As Boolean = False
            Dim regY As Boolean = False
            For Each m As memoryTarget In regReqirements
                If m.realAddress.Type = MemoryType.CPU_REG Then
                    Select Case m.realAddress.ID
                        Case CpuRegister.a
                            regA = True
                        Case CpuRegister.x
                            regX = True
                        Case CpuRegister.y
                            regY = True
                    End Select
                End If
            Next
            If fullCode(i).backSource.Count > 0 And (flgC Or flgZ Or flgI Or flgD Or flgV Or flgN Or regA Or regX Or regY) Then
                tmpTracedAddress.Clear()
                For Each b As UInt32 In fullCode(i).backSource
                    If fullCode(i - 1).realAddress = b Then
                        traceRequireChanges(fullCode(i).realAddress, i - 1, flgC, flgZ, flgI, flgD, flgV, flgN, regA, regX, regY)
                    Else
                        traceRequireChanges(fullCode(i).realAddress, findInstructionIndex(b, 0, fullCode.Count - 1), flgC, flgZ, flgI, flgD, flgV, flgN, regA, regX, regY)
                    End If
                Next
            End If
        Next

        'disable simplify for the time being
        'simplify()

        'split into sections
        codeSections.Clear()
        Dim section As addressRange
        section.rangeStart = fullCode(0).realAddress
        section.rangeEnd = fullCode(fullCode.Count - 1).realAddress
        codeSections.Add(section)


        splitSection(UInt16.MaxValue, resetAddress)
        splitSection(UInt16.MaxValue, nmiAddress)
        If hasBrk And Not brkTraced Then
            splitSection(UInt16.MaxValue, brkAddress)
        End If

        For i As Integer = 0 To fullCode.Count - 1
            If fullCode(i).type = InstructionType.SUBROUTINE Then
                Dim bi As instSubroutine = CType(fullCode(i), instSubroutine)
                splitSection(UInt16.MaxValue, bi.subRealAddress)
            End If
        Next

        Dim sectionAdded As Boolean = True
        While sectionAdded
            sectionAdded = False
            For i As Integer = 0 To fullCode.Count - 1
                Select Case fullCode(i).type
                    Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                        Dim b As instPBranch = CType(fullCode(i), instPBranch)
                        sectionAdded = sectionAdded Or splitSection(b.realAddress, b.branchToAddress)
                    Case InstructionType.JUMP
                        Dim b As instJump = CType(fullCode(i), instJump)
                        sectionAdded = sectionAdded Or splitSection(b.realAddress, b.jumpToRealAddress)
                End Select
            Next
        End While
        codeSections.Sort(New addressRangeDestComparer)

        Dim s As String = ""
        For Each i As instruction In fullCode
            s &= i.printTraceResult
        Next
        frm.txtAnaCode2.Text = s

        s = ""
        For Each i As addressRange In codeSections
            s &= "Section " & realAddressToHexStr(i.rangeStart) & vbCrLf
            Dim idx As Integer = findInstructionIndex(i.rangeStart, 0, fullCode.Count - 1)
            Dim sectionEnded As Boolean = False
            Do Until sectionEnded
                Dim tInst As instruction = fullCode(idx)
                If tInst.realAddress > i.rangeEnd Then
                    sectionEnded = True
                Else
                    For Each b As UInt32 In tInst.backSource
                        If (b < i.rangeStart Or b > i.rangeEnd) And Not tInst.subReturnAddresses.Contains(b) Then
                            s &= "  " & realAddressToHexStr(tInst.realAddress) & " ENT " & realAddressToHexStr(b) & vbCrLf
                        End If
                    Next

                    Select Case tInst.type
                        Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                            Dim b As instPBranch = CType(tInst, instPBranch)
                            s &= "  " & realAddressToHexStr(b.realAddress) & " BCH " & realAddressToHexStr(b.branchToAddress) & vbCrLf
                        Case InstructionType.JUMP
                            Dim b As instJump = CType(tInst, instJump)
                            s &= "  " & realAddressToHexStr(b.realAddress) & " JMP " & realAddressToHexStr(b.jumpToRealAddress) & vbCrLf
                        Case InstructionType.SUBROUTINE
                            Dim b As instSubroutine = CType(tInst, instSubroutine)
                            s &= "  " & realAddressToHexStr(b.realAddress) & " JSR " & realAddressToHexStr(b.subRealAddress) & vbCrLf
                        Case InstructionType.SUB_RETURN
                            Dim b As instSubReturn = CType(tInst, instSubReturn)
                            s &= "  " & realAddressToHexStr(b.realAddress) & " RTS " & IIf(b.returnType = SubReturnType.NORMAL, "Normal", IIf(b.returnType = SubReturnType.SKIP_TO_PREVIOUS, "Skip", "Jump")) & vbCrLf

                    End Select
                End If
                idx += 1
                If idx > fullCode.Count - 1 Then
                    sectionEnded = True
                End If
            Loop
            s &= "End " & realAddressToHexStr(i.rangeEnd) & vbCrLf & vbCrLf
        Next
        frm.txtAnaCode3.Text = s

        convertToBlocks()
        s = ""
        For Each bl As block In blocks
            s &= bl.saveToString
        Next
        frm.txtAnaCode4.Text = s
    End Sub

    Public Function splitSection(sourceAddress As UInt16, jumpAddress As UInt16) As Boolean
        Dim j As Integer = 0
        While j < codeSections.Count
            If codeSections(j).rangeStart < jumpAddress And codeSections(j).rangeEnd >= jumpAddress _
                And (codeSections(j).rangeStart > sourceAddress Or codeSections(j).rangeEnd < sourceAddress) Then

                'this section contains the target, but not the source address
                Dim newSection As addressRange
                newSection.rangeStart = jumpAddress
                newSection.rangeEnd = codeSections(j).rangeEnd
                codeSections.Insert(j + 1, newSection)

                Dim oldSection As addressRange = codeSections(j)
                Dim si As Integer = findInstructionIndex(jumpAddress, 0, fullCode.Count - 1)
                oldSection.rangeEnd = fullCode(si - 1).realAddress
                codeSections(j) = oldSection
                Return True
            End If
            j += 1
        End While
        Return False
    End Function

    Public Sub loadBlocksFromString(s As String)
        Do Until s = ""
            Dim b As New block
            b.loadFromString(s)
            lines.Add(b)
            Select Case b.type
                Case BlockType.RESET
                    resetAddress = b.realAddress
                Case BlockType.NMI
                    nmiAddress = b.realAddress
                Case BlockType.BRK
                    brkAddress = b.realAddress
                    hasBrk = True
            End Select
        Loop
    End Sub

    Private Sub traceBranch(t As traceTask)
        Dim tSetting As New traceSetting
        Dim m As stackEntry
        tSetting.stack.AddRange(t.stack)
        Select Case t.type
            Case TaskType.JSR
                m.source.ID = CpuRegister.pc
                m.source.Type = MemoryType.CPU_REG
                m.name = t.name
                tSetting.stack.Add(m)
                tSetting.stack.Add(m)
            Case TaskType.BRK, TaskType.NMI
                m.source.ID = CpuRegister.pc
                m.source.Type = MemoryType.CPU_REG
                m.name = t.name
                tSetting.stack.Add(m)
                tSetting.stack.Add(m)
                m.source.ID = CpuRegister.p
                tSetting.stack.Add(m)
        End Select
        tSetting.sourceAddress = t.source
        tSetting.startAddress = t.realAddress
        traceSection(t, tSetting)
        If t.type = TaskType.BRK Then
            brkTraced = True
        End If


    End Sub

    Private Sub traceSection(t As traceTask, ts As traceSetting)
        Dim p As Integer = findInstructionIndex(ts.startAddress, 0, fullCode.Count - 1)
        Dim tInst As instruction
        Dim traceEnd As Boolean = False
        Dim furtherTraceAddresses As New List(Of traceSetting)
        Dim skipNextBackAddress As Boolean = False
        Dim backAddress As UInt32 = ts.sourceAddress
        Dim addressPair As traceSetting
        Do Until traceEnd
            tInst = fullCode(p)

            Dim isNew As Boolean = tInst.traceMarking = ""
            Dim traceName As String = t.name & "@" & backAddress.ToString("X6")
            If Not tInst.traceMarking.Contains(traceName) Then
                tInst.traceMarking &= traceName & " "
            Else
                traceEnd = True
            End If
            If skipNextBackAddress Then
                skipNextBackAddress = False
            Else
                If backAddress <> UInt32.MaxValue And Not tInst.backSource.Contains(backAddress) Then
                    tInst.backSource.Add(backAddress)
                End If
            End If

            If Not traceEnd Then
                Select Case tInst.type
                    Case InstructionType.BRANCH
                        Dim b As instBranch = CType(tInst, instBranch)
                        If b.branchToAddress < b.realAddress And b.branchToAddress >= t.realAddress Then
                            'branch to previous code, continue tracing
                            fullCode(findInstructionIndex(b.branchToAddress, 0, fullCode.Count - 1)).backSource.Add(b.realAddress)
                        Else
                            'branch to further code, put address to further trace list
                            addressPair = New traceSetting With {
                                .sourceAddress = b.realAddress,
                                .startAddress = b.branchToAddress
                            }
                            addressPair.stack.AddRange(ts.stack)
                            furtherTraceAddresses.Add(addressPair)
                        End If
                    Case InstructionType.JUMP
                        Dim b As instJump = CType(tInst, instJump)
                        If b.isIndirect Then
                            'not sure what to do yet
                        Else
                            addressPair = New traceSetting With {
                                .sourceAddress = b.realAddress,
                                .startAddress = b.jumpToRealAddress
                            }
                            addressPair.stack.AddRange(ts.stack)
                            furtherTraceAddresses.Add(addressPair)
                        End If
                        traceEnd = True
                    Case InstructionType.SUBROUTINE
                        Dim b As instSubroutine = CType(tInst, instSubroutine)
                        Dim newTask As New traceTask
                        newTask.realAddress = b.subRealAddress
                        newTask.source = tInst.realAddress
                        newTask.stack.AddRange(ts.stack)
                        If Not b.restoreFlags Then
                            newTask.name = "SUB_" & b.subRealAddress.ToString("X6")
                            newTask.type = TaskType.JSR
                        Else
                            newTask.name = "BRK"
                            newTask.type = TaskType.BRK
                        End If
                        If Not traceTaskIsDuplicate(newTask) Then
                            traceTasksToRun.Add(newTask)
                            traceBranch(newTask)
                        Else
                            'use result from the existing task
                            For Each f As traceTask In traceTasksToRun
                                If f.realAddress = b.subRealAddress Then
                                    newTask = f
                                    Exit For
                                End If
                            Next
                        End If

                        If newTask.endWithSkip Then
                            'a sub call return is skipped directly to return this subroutine
                            t.endWithReturn = True
                            t.rtsAddress.AddRange(newTask.rtsAddress)
                        End If
                        If newTask.endWithReturn Then
                            'add rts address to the source of the next instruction
                            fullCode(p + 1).backSource.AddRange(newTask.rtsAddress)
                            fullCode(p + 1).backSource = fullCode(p + 1).backSource.Distinct().ToList()
                            fullCode(p + 1).subReturnAddresses.AddRange(newTask.rtsAddress)
                            fullCode(p + 1).subReturnAddresses = fullCode(p + 1).subReturnAddresses.Distinct().ToList()
                            skipNextBackAddress = True
                            b.hasReturned = True
                        Else
                            traceEnd = True
                        End If


                    Case InstructionType.SUB_RETURN
                        Dim b As instSubReturn = CType(tInst, instSubReturn)
                        If b.restoreFlags Then
                            'pop flags from stack
                            ts.stack.RemoveAt(ts.stack.Count - 1)
                        End If
                        Dim m As stackEntry
                        m = ts.stack(ts.stack.Count - 1)
                        ts.stack.RemoveAt(ts.stack.Count - 1)
                        ts.stack.RemoveAt(ts.stack.Count - 1)
                        If m.source.Type = MemoryType.CPU_REG And m.source.ID = CpuRegister.pc Then
                            If m.name = t.name Then
                                'return to the original code
                                t.endWithReturn = True
                                b.returnType = SubReturnType.NORMAL
                            Else
                                'return twice
                                t.endWithSkip = True
                                If b.returnType = SubReturnType.UNKNOWN Then
                                    b.returnType = SubReturnType.SKIP_TO_PREVIOUS
                                End If
                            End If
                            t.rtsAddress.Add(b.realAddress)
                        Else
                            'indirect jump
                            b.returnType = SubReturnType.INDIRECT_JUMP
                        End If

                        traceEnd = True
                    Case InstructionType.FLAG
                        Dim b As instFlag = CType(tInst, instFlag)
                        If b.updateFlag = FlagID.i Then
                            hasBrk = b.isClear
                        End If
                    Case InstructionType.STACK
                        Dim b As instStack = CType(tInst, instStack)
                        If b.isPush Then
                            Dim m As stackEntry
                            m.source.ID = b.regToKeep
                            m.source.Type = MemoryType.CPU_REG
                            m.name = t.name
                            ts.stack.Add(m)
                        Else
                            'pop from stack
                            ts.stack.RemoveAt(ts.stack.Count - 1)
                        End If
                End Select

                p += 1
                backAddress = tInst.realAddress
            End If

        Loop

        'trace the further addresses
        For i As Integer = 0 To furtherTraceAddresses.Count - 1
            Dim f As traceSetting = furtherTraceAddresses(i)
            Dim p2 As Integer = findInstructionIndex(f.startAddress, 0, fullCode.Count - 1)
            If p2 <> -1 Then
                Dim traceName As String = t.name & "@" & f.sourceAddress.ToString("X6")
                Dim tInst2 As instruction = fullCode(p2)
                If Not tInst2.traceMarking.Contains(traceName) Then
                    traceSection(t, f)
                End If
            End If
        Next

    End Sub


    Private Function findInstructionIndex(pAddress As UInt32, i1 As Integer, i2 As Integer) As Integer
        Dim i As Integer = (i1 + i2) / 2
        If fullCode(i).realAddress = pAddress Then
            Return i
        ElseIf fullCode(i).realAddress < pAddress Then
            If i = i2 Then
                Return -1
            Else
                Return findInstructionIndex(pAddress, i + 1, i2)
            End If
        Else
            If i = i1 Then
                Return -1
            Else
                Return findInstructionIndex(pAddress, i1, i - 1)
            End If
        End If
    End Function


    Private Function findIndexInBlock(pBlock As block, pAddress As UInt32, i1 As Integer, i2 As Integer) As Integer
        Dim i As Integer = (i1 + i2) / 2
        If pBlock.code(i).isBlock Then
            Dim b As block = pBlock.code(i)
            If b.code.Count = 0 Then
                If pBlock.type = BlockType.IF_THEN_ELSE Then
                    If i = 1 Then
                        If CType(pBlock.code(0), instruction).realAddress = pAddress Then
                            Return 0
                        ElseIf pBlock.code.Count > 2 Then
                            Return findIndexInBlock(pBlock, pAddress, i + 1, i2)
                        End If
                    ElseIf i = 2 Then
                        Return findIndexInBlock(pBlock, pAddress, i1, i - 1)
                    End If
                End If
                Return -1 'empty block, no address found
            Else
                Dim r As addressRange = CType(pBlock.code(i), block).codeRange
                If r.rangeStart <= pAddress And r.rangeEnd >= pAddress Then
                    Return i
                ElseIf r.rangeEnd < pAddress Then
                    If i = i2 Then
                        Return -1
                    Else
                        Return findIndexInBlock(pBlock, pAddress, i + 1, i2)
                    End If
                Else
                    If i = i1 Then
                        Return -1
                    Else
                        Return findIndexInBlock(pBlock, pAddress, i1, i - 1)
                    End If
                End If
            End If
        Else
            If pBlock.code(i).realAddress = pAddress Then
                Return i
            ElseIf pBlock.code(i).realAddress < pAddress Then
                If i = i2 Then
                    Return -1
                Else
                    Return findIndexInBlock(pBlock, pAddress, i + 1, i2)
                End If
            Else
                If i = i1 Then
                    Return -1
                Else
                    Return findIndexInBlock(pBlock, pAddress, i1, i - 1)
                End If
            End If
        End If

    End Function



    Private Sub traceRequireChanges(source As UInt32, fromIdx As Integer, fC As Boolean, fZ As Boolean, fI As Boolean, fD As Boolean, fV As Boolean, fN As Boolean, rA As Boolean, rX As Boolean, rY As Boolean)
        Dim traceEnd As Boolean = False
        Do Until traceEnd
            Dim tInst As instruction = fullCode(fromIdx)
            If tInst.flgCarryChange And fC Then
                If tInst.flgCReqAddress < source Or tInst.flgCReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgCReqAddress = source
                End If
                fC = False
            End If
            If tInst.flgZeroChange And fZ Then
                If tInst.flgZReqAddress < source Or tInst.flgZReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgZReqAddress = source
                End If
                fZ = False
            End If
            If tInst.flgIntrDisChange And fI Then
                If tInst.flgIReqAddress < source Or tInst.flgIReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgIReqAddress = source
                End If
                fI = False
            End If
            If tInst.flgDecimalChange And fD Then
                If tInst.flgDReqAddress < source Or tInst.flgDReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgDReqAddress = source
                End If
                fD = False
            End If
            If tInst.flgOverflowChange And fV Then
                If tInst.flgVReqAddress < source Or tInst.flgVReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgVReqAddress = source
                End If
                fV = False
            End If
            If tInst.flgNegativeChange And fN Then
                If tInst.flgNReqAddress < source Or tInst.flgNReqAddress = UInt32.MaxValue Or tInst.realAddress > source Then
                    tInst.flgNReqAddress = source
                End If
                fN = False
            End If
            Dim tList As List(Of memoryTarget) = tInst.getOverwrittenMemoryTarget
            For Each m As memoryTarget In tList
                If m.realAddress.Type = MemoryType.CPU_REG Then
                    Select Case m.realAddress.ID
                        Case CpuRegister.a
                            If rA And (tInst.regAReqAddress < source Or tInst.regAReqAddress = UInt32.MaxValue Or tInst.realAddress > source) Then
                                tInst.regAReqAddress = source
                            End If
                            rA = False
                        Case CpuRegister.x
                            If rX And (tInst.regXReqAddress < source Or tInst.regXReqAddress = UInt32.MaxValue Or tInst.realAddress > source) Then
                                tInst.regXReqAddress = source
                            End If
                            rX = False
                        Case CpuRegister.y
                            If rY And (tInst.regYReqAddress < source Or tInst.regYReqAddress = UInt32.MaxValue Or tInst.realAddress > source) Then
                                tInst.regYReqAddress = source
                            End If
                            rY = False
                    End Select
                End If
            Next
            tmpTracedAddress.Add(tInst.realAddress)
            Dim hasBackSource As Boolean = False
            If tInst.backSource.Count > 0 And (fC Or fZ Or fI Or fD Or fV Or fN Or rA Or rX Or rY) Then
                'check if one of the back sources is the previous instruction
                For Each b As UInt32 In tInst.backSource
                    If b <> source And Not tmpTracedAddress.Contains(b) Then 'avoid infinite loop
                        Dim isBack As Boolean = False
                        If fromIdx > 0 And Not hasBackSource Then
                            If fullCode(fromIdx - 1).realAddress = b Then
                                isBack = True
                                hasBackSource = True
                                fromIdx -= 1
                            End If
                        End If
                        If Not isBack Then
                            traceRequireChanges(source, findInstructionIndex(b, 0, fullCode.Count - 1), fC, fZ, fI, fD, fV, fN, rA, rX, rY)
                        End If
                    End If
                Next
            End If
            If Not hasBackSource Then
                traceEnd = True
            End If
        Loop
    End Sub

    Private Sub simplify()
        'simplyfy the code
        For i As Integer = 0 To fullCode.Count - 1
            If i + 1 <= fullCode.Count - 1 Then

                'check for transfer instructions that can be combined
                If fullCode(i).type = InstructionType.TRANSFER And fullCode(i + 1).type = InstructionType.TRANSFER And Not fullCode(i + 1).isJumpTarget Then
                    Dim b As instTransfer = CType(fullCode(i), instTransfer)
                    Dim b2 As instTransfer = CType(fullCode(i + 1), instTransfer)
                    If b.destination.addrMode = AddressingMode.IMPLICIT And b2.source.addrMode = AddressingMode.IMPLICIT _
                        And b.destination.realAddress.Type = MemoryType.CPU_REG And b2.source.realAddress.Type = MemoryType.CPU_REG _
                        And b.destination.realAddress.ID = b2.source.realAddress.ID Then
                        'check if the transfer to register is overwritten before being used later
                        Dim regNotRequired As Boolean = False
                        Select Case b.destination.realAddress.ID
                            Case CpuRegister.a
                                If b.regAReqAddress = b2.realAddress Or b.regAReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.x
                                If b.regXReqAddress = b2.realAddress Or b.regXReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.y
                                If b.regYReqAddress = b2.realAddress Or b.regYReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                        End Select
                        Dim flagNotRequired As Boolean = False
                        If (b.flgZReqAddress = b2.realAddress Or b.flgZReqAddress = UInt32.MaxValue) _
                            And (b.flgNReqAddress = b2.realAddress Or b.flgNReqAddress = UInt32.MaxValue) Then
                            flagNotRequired = True
                        End If

                        If flagNotRequired And regNotRequired Then
                            'can be combined
                            b.destination.realAddress = b2.destination.realAddress
                            b.destination.addrMode = b2.destination.addrMode
                            b.destination.address = b2.destination.address
                            b.opName = b.opName & "+" & b2.opName
                            fullCode(i + 2).backSource.Add(b.realAddress)
                            fullCode(i + 2).backSource.Remove(b2.realAddress)
                            fullCode.RemoveAt(i + 1)
                        End If
                    End If
                End If

                'check for transfer and compare instructions that can be combined
                If fullCode(i).type = InstructionType.TRANSFER And fullCode(i + 1).type = InstructionType.COMPARE And Not fullCode(i + 1).isJumpTarget Then
                    Dim b As instTransfer = CType(fullCode(i), instTransfer)
                    Dim b2 As instCompare = CType(fullCode(i + 1), instCompare)
                    If b.destination.addrMode = AddressingMode.IMPLICIT And b.destination.realAddress.Type = MemoryType.CPU_REG _
                        And b.destination.realAddress.ID = b2.operand1 Then
                        'check if the transfer to register is overwritten before being used later
                        'any flag changes are overwritten by the compare instruction
                        Dim regNotRequired As Boolean = False
                        Select Case b.destination.realAddress.ID
                            Case CpuRegister.a
                                If b.regAReqAddress = b2.realAddress Or b.regAReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.x
                                If b.regXReqAddress = b2.realAddress Or b.regXReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.y
                                If b.regYReqAddress = b2.realAddress Or b.regYReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                        End Select

                        If regNotRequired Then
                            'can be combined
                            Dim newCompare As New instDirectCompare With {
                                .realAddress = b.realAddress,
                                .operand1 = b.source,
                                .operand2 = b2.operand2,
                                .opName = b.opName & "+" & b2.opName,
                                .flgCReqAddress = b2.flgCReqAddress,
                                .flgZReqAddress = b2.flgZReqAddress,
                                .flgNReqAddress = b2.flgNReqAddress,
                                .regAReqAddress = b.regAReqAddress,
                                .regXReqAddress = b.regXReqAddress,
                                .regYReqAddress = b.regYReqAddress,
                                .isJumpTarget = b.isJumpTarget,
                                .traceMarking = b.traceMarking
                            }
                            newCompare.backSource.AddRange(b.backSource)
                            newCompare.subReturnAddresses.AddRange(b.subReturnAddresses)
                            fullCode(i) = newCompare
                            fullCode(i + 2).backSource.Add(b.realAddress)
                            fullCode(i + 2).backSource.Remove(b2.realAddress)
                            fullCode.RemoveAt(i + 1)
                        End If
                    End If
                End If

                'check for compare and branch instructions that can be combined
                If fullCode(i).type = InstructionType.COMPARE And fullCode(i + 1).type = InstructionType.BRANCH And Not fullCode(i + 1).isJumpTarget Then
                    Dim b As instCompare = CType(fullCode(i), instCompare)
                    Dim b2 As instBranch = CType(fullCode(i + 1), instBranch)

                    Dim flagNotRequired As Boolean = False
                    If b2.flgZReqAddress = UInt32.MaxValue And b2.flgNReqAddress = UInt32.MaxValue And b2.flgCReqAddress = UInt32.MaxValue Then
                        flagNotRequired = True
                    End If

                    If flagNotRequired Then
                        'can be combined
                        Dim newBranch As New instCompareBranch With {
                            .realAddress = b.realAddress,
                            .operand1 = codeBlock.createCPURegisterMemoryTarget(b.operand1),
                            .operand2 = b.operand2,
                            .opName = b.opName & "+" & b2.opName,
                            .branchToAddress = b2.branchToAddress,
                            .useFlag = b2.useFlag,
                            .flagIsSet = b2.flagIsSet,
                            .isJumpTarget = b.isJumpTarget,
                            .traceMarking = b.traceMarking
                        }
                        newBranch.backSource.AddRange(b.backSource)
                        newBranch.subReturnAddresses.AddRange(b.subReturnAddresses)
                        If b.flgCReqAddress <> b2.realAddress Then newBranch.flgCReqAddress = b.flgCReqAddress
                        If b.flgZReqAddress <> b2.realAddress Then newBranch.flgZReqAddress = b.flgZReqAddress
                        If b.flgNReqAddress <> b2.realAddress Then newBranch.flgNReqAddress = b.flgNReqAddress
                        fullCode(i) = newBranch
                        fullCode(i + 2).backSource.Add(b.realAddress)
                        fullCode(i + 2).backSource.Remove(b2.realAddress)
                        fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Add(b.realAddress)
                        fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Remove(b2.realAddress)
                        fullCode.RemoveAt(i + 1)
                    End If

                End If

                'check for direct compare and branch instructions that can be combined
                If fullCode(i).type = InstructionType.DIRECT_COMPARE And fullCode(i + 1).type = InstructionType.BRANCH And Not fullCode(i + 1).isJumpTarget Then
                    Dim b As instDirectCompare = CType(fullCode(i), instDirectCompare)
                    Dim b2 As instBranch = CType(fullCode(i + 1), instBranch)

                    Dim flagNotRequired As Boolean = False
                    If b2.flgZReqAddress = UInt32.MaxValue And b2.flgNReqAddress = UInt32.MaxValue And b2.flgCReqAddress = UInt32.MaxValue Then
                        flagNotRequired = True
                    End If
                    If flagNotRequired Then
                        'can be combined
                        Dim newBranch As New instCompareBranch With {
                            .realAddress = b.realAddress,
                            .operand1 = b.operand1,
                            .operand2 = b.operand2,
                            .opName = b.opName & "+" & b2.opName,
                            .branchToAddress = b2.branchToAddress,
                            .useFlag = b2.useFlag,
                            .flagIsSet = b2.flagIsSet,
                            .isJumpTarget = b.isJumpTarget,
                            .traceMarking = b.traceMarking
                        }
                        newBranch.backSource.AddRange(b.backSource)
                        newBranch.subReturnAddresses.AddRange(b.subReturnAddresses)
                        If b.flgCReqAddress <> b2.realAddress Then newBranch.flgCReqAddress = b.flgCReqAddress
                        If b.flgZReqAddress <> b2.realAddress Then newBranch.flgZReqAddress = b.flgZReqAddress
                        If b.flgNReqAddress <> b2.realAddress Then newBranch.flgNReqAddress = b.flgNReqAddress
                        fullCode(i) = newBranch
                        fullCode(i + 2).backSource.Add(b.realAddress)
                        fullCode(i + 2).backSource.Remove(b2.realAddress)
                        fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Add(b.realAddress)
                        fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Remove(b2.realAddress)
                        fullCode.RemoveAt(i + 1)
                    End If

                End If

                'check for load and branch instructions that can be combined
                If fullCode(i).type = InstructionType.TRANSFER And fullCode(i + 1).type = InstructionType.BRANCH And Not fullCode(i + 1).isJumpTarget Then
                    Dim b As instTransfer = CType(fullCode(i), instTransfer)
                    Dim b2 As instBranch = CType(fullCode(i + 1), instBranch)
                    If b.destination.addrMode = AddressingMode.IMPLICIT And b.destination.realAddress.Type = MemoryType.CPU_REG Then
                        'check if the transfer to register is overwritten before being used later
                        'any flag changes are overwritten by the compare instruction
                        Dim regNotRequired As Boolean = False
                        Select Case b.destination.realAddress.ID
                            Case CpuRegister.a
                                If b.regAReqAddress = b2.realAddress Or b.regAReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.x
                                If b.regXReqAddress = b2.realAddress Or b.regXReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                            Case CpuRegister.y
                                If b.regYReqAddress = b2.realAddress Or b.regYReqAddress = UInt32.MaxValue Then
                                    regNotRequired = True
                                End If
                        End Select
                        Dim flagNotRequired As Boolean = False
                        If (b.flgZReqAddress = b2.realAddress Or b.flgZReqAddress = UInt32.MaxValue) _
                            And (b.flgNReqAddress = b2.realAddress Or b.flgNReqAddress = UInt32.MaxValue) _
                            And b2.flgZReqAddress = UInt32.MaxValue And b2.flgNReqAddress = UInt32.MaxValue Then
                            flagNotRequired = True
                        End If

                        If flagNotRequired And regNotRequired Then
                            'can be combined
                            Dim newBranch As New instLoadBranch With {
                                .realAddress = b.realAddress,
                                .operand = b.source,
                                .opName = b.opName & "+" & b2.opName,
                                .branchToAddress = b2.branchToAddress,
                                .useFlag = b2.useFlag,
                                .flagIsSet = b2.flagIsSet,
                                .isJumpTarget = b.isJumpTarget,
                                .traceMarking = b.traceMarking
                            }
                            newBranch.backSource.AddRange(b.backSource)
                            newBranch.subReturnAddresses.AddRange(b.subReturnAddresses)
                            If b.flgZReqAddress <> b2.realAddress Then newBranch.flgZReqAddress = b.flgZReqAddress
                            If b.flgNReqAddress <> b2.realAddress Then newBranch.flgNReqAddress = b.flgNReqAddress
                            fullCode(i) = newBranch
                            fullCode(i + 2).backSource.Add(b.realAddress)
                            fullCode(i + 2).backSource.Remove(b2.realAddress)
                            fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Add(b.realAddress)
                            fullCode(findInstructionIndex(b2.branchToAddress, 0, fullCode.Count - 1)).backSource.Remove(b2.realAddress)
                            fullCode.RemoveAt(i + 1)
                        End If
                    End If
                End If

                'check for repeated modify instructions that can be combined
                If fullCode(i).type = InstructionType.MODIFY Then
                    Dim b As instModify = CType(fullCode(i), instModify)
                    Dim canRepeat As Boolean = True
                    Dim repeatCount As Integer = 0
                    Do Until canRepeat = False Or i + repeatCount + 1 > fullCode.Count - 1 Or repeatCount = 8
                        If fullCode(i + repeatCount + 1).type <> InstructionType.MODIFY Or fullCode(i + repeatCount + 1).isJumpTarget Then
                            canRepeat = False
                        Else
                            Dim b2 As instModify = CType(fullCode(i + repeatCount + 1), instModify)
                            If b.opName = b2.opName And b.operand.addrMode = b2.operand.addrMode And b.operand.realAddress.Type = b2.operand.realAddress.Type And b.operand.realAddress.ID = b2.operand.realAddress.ID Then
                                repeatCount += 1
                            Else
                                canRepeat = False
                            End If
                        End If
                    Loop
                    If repeatCount > 0 Then
                        Dim newModify As New instRepeatedModify With {
                            .realAddress = b.realAddress,
                            .operand = b.operand,
                            .opName = b.opName & "x" & (repeatCount + 1).ToString,
                            .isJumpTarget = b.isJumpTarget,
                            .traceMarking = b.traceMarking,
                            .requireFlagC = b.requireFlagC,
                            .flgCReqAddress = fullCode(i + repeatCount).flgCReqAddress,
                            .flgZReqAddress = fullCode(i + repeatCount).flgZReqAddress,
                            .flgNReqAddress = fullCode(i + repeatCount).flgNReqAddress,
                            .regAReqAddress = fullCode(i + repeatCount).regAReqAddress,
                            .regXReqAddress = fullCode(i + repeatCount).regXReqAddress,
                            .regYReqAddress = fullCode(i + repeatCount).regYReqAddress,
                            .repeatedTimes = repeatCount + 1
                        }
                        newModify.backSource.AddRange(b.backSource)
                        newModify.subReturnAddresses.AddRange(b.subReturnAddresses)
                        fullCode(i) = newModify
                        fullCode(i + repeatCount + 1).backSource.Add(b.realAddress)
                        fullCode(i + repeatCount + 1).backSource.Remove(fullCode(i + repeatCount).realAddress)
                        For j As Integer = 0 To repeatCount - 1
                            fullCode.RemoveAt(i + 1)
                        Next
                    End If
                End If

            End If
        Next
    End Sub

    Private Sub convertToBlocks()
        Dim sdx As Integer
        blocks.Clear()

        For Each i As addressRange In codeSections
            Dim idx As Integer = findInstructionIndex(i.rangeStart, 0, fullCode.Count - 1)
            Dim idxEnd As Integer = findInstructionIndex(i.rangeEnd, 0, fullCode.Count - 1)

            'fill block with code
            Dim newBlock As New block
            Dim tInst As instruction
            Dim lastInstruction As instruction
            Select Case i.rangeStart
                Case resetAddress
                    newBlock.type = BlockType.RESET
                    newBlock.name = "RESET"
                Case nmiAddress
                    newBlock.type = BlockType.NMI
                    newBlock.name = "NMI"
                Case brkAddress
                    newBlock.type = BlockType.BRK
                    newBlock.name = "BRK"
                Case Else
                    newBlock.type = BlockType.SUBROUNTINE
                    newBlock.name = "SUB_" & i.rangeStart.ToString("X6")
            End Select
            tInst = fullCode(idx)
            newBlock.realAddress = tInst.realAddress
            newBlock.addCodeBlock(tInst)
            idx += 1
            Do Until idx > idxEnd
                lastInstruction = tInst
                tInst = fullCode(idx)
                If newBlock.code.Count > 0 Then
                    'if last instruction is in the same section
                    lastInstruction.nextAddress = tInst.realAddress
                End If
                newBlock.addCodeBlock(tInst)
                idx += 1
            Loop

            'find loops and check for conflicts
            Dim structureList As New List(Of structRange)
            Dim cdx As Integer = 0
            While cdx <= newBlock.code.Count - 1
                tInst = newBlock.code(cdx)
                Select Case tInst.type
                    Case InstructionType.JUMP
                        Dim tJump As instJump = tInst
                        If tJump.jumpToRealAddress < tJump.realAddress And tJump.jumpToRealAddress >= i.rangeStart And Not tJump.isIndirect Then
                            If Not tInst.isJumpTarget Then
                                Dim tStruct As structRange
                                tStruct.type = BlockType.LOOP_CONTENT
                                tStruct.nextAddress = UInt32.MaxValue
                                'add first inst
                                Dim bIdx As Integer = findIndexInBlock(newBlock, tJump.jumpToRealAddress, 0, newBlock.code.Count - 1)
                                If bIdx <> -1 Then
                                    tStruct.rangeStart = newBlock.code(bIdx).realAddress
                                    If bIdx < newBlock.code.Count - 1 Then
                                        tStruct.nextAddress = newBlock.code(bIdx + 1).realAddress
                                    End If
                                End If
                                tStruct.rangeEnd = tJump.realAddress
                                structureList.Add(tStruct)
                            End If
                        End If
                    Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                        Dim tBranch As instPBranch = tInst
                        If tBranch.branchToAddress < tBranch.realAddress And tBranch.branchToAddress >= i.rangeStart Then
                            If Not tInst.isJumpTarget Then
                                Dim tStruct As structRange
                                tStruct.type = BlockType.BRANCH_LOOP
                                'add first inst
                                Dim bIdx As Integer = findIndexInBlock(newBlock, tBranch.branchToAddress, 0, newBlock.code.Count - 1)
                                If bIdx <> -1 Then
                                    tStruct.rangeStart = newBlock.code(bIdx).realAddress
                                    If bIdx < newBlock.code.Count - 1 Then
                                        tStruct.nextAddress = newBlock.code(bIdx + 1).realAddress
                                    End If
                                End If
                                tStruct.rangeEnd = tBranch.realAddress
                                structureList.Add(tStruct)
                            End If
                        End If
                End Select
                cdx += 1
            End While
            'check for conflicts
            sdx = 0
            While sdx <= structureList.Count - 2
                Dim tStruct As structRange = structureList(sdx)
                Dim tdx = sdx + 1
                While tdx <= structureList.Count - 1
                    Dim tStruct2 As structRange = structureList(tdx)
                    If Not ((tStruct.rangeStart <= tStruct2.rangeStart And tStruct.rangeEnd > tStruct2.rangeEnd) _
                           Or (tStruct2.rangeStart <= tStruct.rangeStart And tStruct2.rangeEnd > tStruct.rangeEnd) _
                           Or tStruct.rangeStart > tStruct2.rangeEnd Or tStruct.rangeEnd < tStruct2.rangeStart) Then
                        'keep the earlier one
                        If tStruct.rangeEnd < tStruct2.rangeEnd Then
                            structureList.RemoveAt(tdx)
                            tdx -= 1
                        Else
                            structureList.RemoveAt(sdx)
                            sdx -= 1
                            tdx = structureList.Count
                        End If
                    End If
                    tdx += 1
                End While
                sdx += 1
            End While

            'create loop blocks
            For Each tStruct As structRange In structureList
                convertToLoopBlock(newBlock, tStruct)
            Next
            'detect break from loop
            For Each tStruct As structRange In structureList
                detectBreakFromLoop(newBlock, tStruct)
            Next
            'detect infinite loop
            For Each tStruct As structRange In structureList
                Dim hasExit As Boolean = False
                Dim tBlock As block = findLoopInBlock(newBlock, tStruct)
                If Not tBlock.code(tBlock.code.Count - 1).isBlock Then
                    If CType(tBlock.code(tBlock.code.Count - 1), instruction).type <> InstructionType.JUMP Then
                        hasExit = True
                    End If
                End If
                If Not hasExit And Not detectHasExitFromLoop(tBlock, tStruct) Then
                    Dim cIdx As Integer = findIndexInBlock(newBlock, tStruct.rangeStart, 0, newBlock.code.Count - 1)
                    If cIdx <> -1 Then
                        Dim b As block = newBlock.code(cIdx)
                        newBlock.code.RemoveAt(cIdx)
                        Dim newLoop As New block With {
                            .type = BlockType.INFINITE_LOOP,
                            .name = "INFINITE_LOOP_" & tStruct.rangeStart.ToString("X6"),
                            .realAddress = tStruct.rangeStart
                        }
                        newLoop.addCodeBlock(b.code(0))
                        blocks.Add(newLoop)
                    End If
                End If
            Next

            'check for if-else structures
            structureList.Clear()
            handleIfThenElse(newBlock, newBlock.codeRange.rangeEnd)


            blocks.Add(newBlock)
        Next

        'sort blocks by start address
        blocks.Sort(Function(a, b) a.codeRange.rangeStart.CompareTo(b.codeRange.rangeStart))

        'convert branches or jumps to other sections into subroutine calls
        'convert remaining branches and jumps to goto
        For Each c As block In blocks
            convertBranchJumpSub(c, c, c.codeRange)
        Next


        'add jump blocks to each block if the block does not return or loop back
        sdx = 0
        While sdx < blocks.Count - 1
            Dim tblock As block = blocks(sdx)
            Dim tblock2 As block = blocks(sdx + 1)
            Dim lastCode As codeBlock = tblock.code(tblock.code.Count - 1)
            Dim needsJump As Boolean = True
            If lastCode.isBlock Then
                Dim bloc As block = lastCode
                Select Case bloc.type
                    Case BlockType.IF_THEN_ELSE, BlockType.BRANCH_LOOP
                        needsJump = True
                    Case Else
                        needsJump = False
                End Select
            Else
                Dim tInst As instruction = lastCode
                Select Case tInst.type
                    Case InstructionType.JUMP_BLOCK
                        Dim jInst As instJumpBlock = tInst
                        If jInst.jumpType <> JumpBlockType.JSR Then
                            needsJump = False
                        End If
                    Case InstructionType.JUMP, InstructionType.SUB_RETURN
                        needsJump = False
                    Case Else
                        needsJump = True
                End Select
            End If
            If needsJump And tblock2.type <> BlockType.INFINITE_LOOP Then
                Dim newJump As New instJumpBlock
                newJump.opName = "JBL"
                newJump.realAddress = getLastInst(tblock).realAddress + 1
                newJump.jumpType = JumpBlockType.JMP
                newJump.blockName = "SUB_" & tblock2.codeRange.rangeStart.ToString("X6")
                tblock.addCodeBlock(newJump)
            End If

            sdx += 1
        End While

        ''merge blocks with single source address
        'Dim j As Integer = 0
        'While j < blocks.Count
        '    Dim hasMerged As Boolean = False
        '    Dim b As block = blocks(j)
        '    If b.type = BlockType.SUBROUNTINE Then
        '        Dim callCnt As Integer = 0
        '        For Each c As codeBlock In b.code
        '            If c.isBlock Then
        '                callCnt += findCallCnt(b.name, c)
        '            End If
        '        Next
        '        If callCnt = 1 Then
        '            'can merge
        '            'find out where it is called
        '            Dim k As Integer = 0
        '            While k < blocks.Count
        '                Dim callBlock As block
        '                Dim callIdx As Integer
        '                If findJBLInBlock(b.name, blocks(k), callBlock, callIdx) Then
        '                    'check it is a jump
        '                    Dim jInst As instJumpBlock = callBlock.code(callIdx)
        '                    If jInst.jumpType = JumpBlockType.JMP Then
        '                        'directly replace the jump
        '                        callBlock.code.RemoveAt(callIdx)
        '                        callBlock.code.InsertRange(callIdx, b.code)
        '                        blocks.RemoveAt(j)
        '                        hasMerged = True
        '                    End If
        '                End If
        '                If hasMerged Then
        '                    k = blocks.Count
        '                Else
        '                    k += 1
        '                End If
        '            End While

        '        End If

        '    End If
        '    If Not hasMerged Then
        '        j += 1
        '    End If
        'End While

        ''merge blocks with single instruction
        'j = 0
        'While j < blocks.Count
        '    Dim hasMerged As Boolean = False
        '    Dim b As block = blocks(j)
        '    If b.type = BlockType.SUBROUNTINE Then
        '        If b.code.Count = 1 Then
        '            If Not b.code(0).isBlock Then
        '                For Each c As block In blocks
        '                    replaceCall(b.name, c, b.code(0))
        '                Next
        '                blocks.RemoveAt(j)
        '                hasMerged = True
        '            End If
        '        End If
        '    End If
        '    If Not hasMerged Then
        '        j += 1
        '    End If
        'End While

    End Sub

    Private Sub convertBranchJumpSub(pBlock As block, pParentBlock As block, pRange As addressRange)
        Dim sdx As Integer = 0
        While sdx < pBlock.code.Count
            If pBlock.code(sdx).isBlock Then
                Dim b As block = pBlock.code(sdx)
                Select Case b.type
                    Case BlockType.IF_THEN_ELSE
                        convertBranchJumpSub(b.code(1), pParentBlock, pRange)
                        If b.code.Count > 2 Then
                            convertBranchJumpSub(b.code(2), pParentBlock, pRange)
                        End If
                    Case BlockType.BRANCH_LOOP
                        'handle branch loop
                        If b.code(0).isBlock Then
                            convertBranchJumpSub(b.code(0), pParentBlock, pRange)
                        Else
                            convertBranchJumpSub(b.code(1), pParentBlock, pRange)
                        End If
                    Case Else
                        convertBranchJumpSub(b, pParentBlock, pRange)
                End Select
            Else
                Select Case CType(pBlock.code(sdx), instruction).type
                    Case InstructionType.JUMP
                        Dim jInst As instJump = pBlock.code(sdx)
                        If pBlock.type <> BlockType.BRANCH_LOOP Then
                            'this is not the loop back inst of a loop
                            Dim newJump As New instJumpBlock
                            newJump.opName = "JBL"
                            newJump.realAddress = jInst.realAddress
                            newJump.backSource.AddRange(jInst.backSource)
                            newJump.subReturnAddresses.AddRange(jInst.subReturnAddresses)
                            newJump.traceMarking = jInst.traceMarking
                            newJump.isJumpTarget = jInst.isJumpTarget
                            newJump.needLabel = jInst.needLabel
                            If jInst.jumpToRealAddress < pRange.rangeStart Or jInst.jumpToRealAddress > pRange.rangeEnd Then
                                'convert to jump block
                                newJump.jumpType = JumpBlockType.JMP
                                newJump.blockName = "SUB_" & jInst.jumpToRealAddress.ToString("X6")
                            Else
                                'convert to goto
                                markLabel(pParentBlock, jInst.jumpToRealAddress)
                                newJump.jumpType = JumpBlockType.JGT
                                newJump.blockName = "L_" & jInst.jumpToRealAddress.ToString("X6")
                            End If
                            pBlock.code(sdx) = newJump
                        End If
                    Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                        Dim binst As instPBranch = pBlock.code(sdx)
                        If pBlock.type <> BlockType.IF_THEN_ELSE Then
                            Dim newBranch As block
                            If binst.branchToAddress < pRange.rangeStart Or binst.branchToAddress > pRange.rangeEnd Then
                                newBranch = createIfJumpBlock(binst, JumpBlockType.JMP)
                            Else
                                markLabel(pParentBlock, binst.branchToAddress)
                                newBranch = createIfJumpBlock(binst, JumpBlockType.JGT)
                            End If
                            pBlock.code(sdx) = newBranch
                        End If
                    Case InstructionType.SUBROUTINE
                        Dim bInst As instSubroutine = pBlock.code(sdx)
                        Dim i As New instJumpBlock
                        i.backSource.AddRange(bInst.backSource)
                        i.blockName = "SUB_" & bInst.subRealAddress.ToString("X6")
                        i.isJumpTarget = bInst.isJumpTarget
                        If bInst.restoreFlags Then
                            i.jumpType = JumpBlockType.BRK
                        Else
                            i.jumpType = JumpBlockType.JSR
                        End If
                        i.opName = "JSB"
                        i.realAddress = bInst.realAddress
                        i.needLabel = bInst.needLabel
                        pBlock.code(sdx) = i
                End Select
            End If
            sdx += 1
        End While
    End Sub

    Private Sub markLabel(pBlock As block, address As UInt32)
        Dim bIdx As Integer = findIndexInBlock(pBlock, address, 0, pBlock.code.Count - 1)
        If bIdx >= 0 Then
            If pBlock.code(bIdx).isBlock Then
                markLabel(pBlock.code(bIdx), address)
            Else
                Dim b As instruction = pBlock.code(bIdx)
                b.needLabel = True
            End If
        End If
    End Sub

    Private Sub handleIfThenElse(pBlock As block, pRangeEnd As UInt32)
        Dim sdx As Integer = 0
        Dim tList As List(Of structRange) = New List(Of structRange)
        While sdx < pBlock.code.Count
            If pBlock.code(sdx).isBlock Then
                Dim tBlock As block = pBlock.code(sdx)
                Select Case tBlock.type
                    Case BlockType.BRANCH_LOOP
                        If tBlock.code(0).isBlock Then
                            handleIfThenElse(tBlock.code(0), pRangeEnd)
                        Else
                            handleIfThenElse(tBlock.code(1), pRangeEnd)
                        End If
                    Case BlockType.IF_THEN_ELSE
                        handleIfThenElse(tBlock.code(1), pRangeEnd)
                        If tBlock.code.Count > 2 Then
                            handleIfThenElse(tBlock.code(2), pRangeEnd)
                        End If
                End Select
            Else
                Select Case CType(pBlock.code(sdx), instruction).type
                    Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                        Dim tInst As instPBranch = CType(pBlock.code(sdx), instPBranch)
                        If tInst.branchToAddress > tInst.realAddress And tInst.branchToAddress <= pRangeEnd Then
                            Dim cIdx As Integer = findIndexInBlock(pBlock, tInst.branchToAddress, sdx, pBlock.code.Count - 1)
                            If cIdx > sdx Then
                                Dim tStruct As structRange
                                tStruct.type = BlockType.IF_THEN_ELSE
                                tStruct.rangeStart = tInst.realAddress
                                tStruct.nextAddress = tInst.branchToAddress
                                'check end address is valid
                                Dim endValid As Boolean = True
                                If pBlock.code(cIdx).isBlock Then
                                    If CType(pBlock.code(cIdx), block).codeRange.rangeStart <> tInst.branchToAddress Then
                                        'ended inside a block, so end address is not valid
                                        endValid = False
                                    End If
                                End If
                                If endValid Then
                                    tStruct.rangeEnd = pBlock.code(cIdx - 1).realAddress
                                    tStruct.combinedEnd = tStruct.rangeEnd
                                    tStruct.hasElse = False

                                    'check for else part
                                    If Not pBlock.code(cIdx - 1).isBlock Then
                                        Dim tI As instruction = pBlock.code(cIdx - 1)
                                        If tI.type = InstructionType.JUMP And Not tI.isJumpTarget Then
                                            Dim tJump As instJump = tI
                                            If Not tJump.isIndirect And tJump.jumpToRealAddress > tInst.realAddress And tJump.jumpToRealAddress <= pRangeEnd Then
                                                'found else part
                                                Dim eIdx As Integer = findIndexInBlock(pBlock, tJump.jumpToRealAddress, cIdx, pBlock.code.Count - 1)
                                                If eIdx <> -1 Then
                                                    'check if else part is valid
                                                    Dim elseValid As Boolean = True
                                                    If pBlock.code(eIdx).isBlock Then
                                                        If CType(pBlock.code(eIdx), block).codeRange.rangeStart <> tJump.jumpToRealAddress Then
                                                            'ended inside a block, so end address is not valid
                                                            elseValid = False
                                                        End If
                                                    End If
                                                    If elseValid Then
                                                        tStruct.hasElse = True
                                                        tStruct.elseRangeStart = tInst.branchToAddress
                                                        tStruct.elseRangeEnd = pBlock.code(eIdx - 1).realAddress
                                                        tStruct.elseNextAddress = tJump.jumpToRealAddress
                                                        tStruct.combinedEnd = tStruct.elseRangeEnd
                                                    End If
                                                End If
                                            End If
                                        End If
                                    End If

                                    tList.Add(tStruct)
                                End If

                            End If
                        End If
                End Select

            End If
            sdx += 1
        End While

        'resolve conflicts
        sdx = 0
        While sdx < tList.Count - 1
            Dim tStruct As structRange = tList(sdx)
            Dim tdx As Integer = sdx + 1
            'structures are sorted by rangeStart, so we can check for conflicts
            While tdx < tList.Count
                Dim tStruct2 As structRange = tList(tdx)
                If tStruct.combinedEnd >= tStruct2.rangeStart Then
                    'conflict found, resolve it
                    If tStruct2.rangeEnd <= tStruct.rangeEnd Then
                        'check for else part
                        If tStruct2.hasElse Then
                            If tStruct2.elseRangeEnd > tStruct.rangeEnd Then
                                'both else invalid
                                tStruct.hasElse = False
                                tStruct2.hasElse = False
                                tList(sdx) = tStruct
                                tList(tdx) = tStruct2
                            End If
                        End If
                    ElseIf tStruct2.rangeStart <= tStruct.rangeEnd Then
                        'tStruct2 starts in tStruct and tStruct2 if block ends after tStruct if block ends
                        'tStruct2 invalid, remove it
                        tList.RemoveAt(tdx)
                        tdx -= 1
                    ElseIf tStruct.elseRangeEnd < tStruct2.combinedEnd Then
                        'tStruct2 starts in else block and tStruct2 ends after tStruct ends
                        If tStruct2.rangeEnd > tStruct.elseRangeEnd Then
                            'tstruct2 if block ends after tStruct else block ends
                            'tStruct2 is invalid, remove it
                            tList.RemoveAt(tdx)
                            tdx -= 1
                        Else
                            'tStruct2 starts in else block and tStruct2 else block ends after tStruct else block ends
                            'tStruct2 else block invalid
                            tStruct2.hasElse = False
                            tList(tdx) = tStruct2
                        End If
                    End If

                End If
                tdx += 1
            End While
            sdx += 1
        End While

        'convert to if blocks
        sdx = 0
        While sdx < tList.Count - 1
            Dim tStruct As structRange = tList(sdx)
            convertToIfBlock(pBlock, tStruct)
            sdx += 1
        End While
    End Sub


    Private Function extractIfBlock(b As block, fromIdx As Integer, pStruct As structRange) As block
        Dim newBlock As New block
        newBlock.type = BlockType.IF_THEN_ELSE

        Dim b1 As New block
        b1.type = BlockType.IF_BLOCK

        Dim b2 As New block
        b2.type = BlockType.Else_BLOCK

        newBlock.addCodeBlock(b1)

        Dim hasWork As Boolean = fromIdx < b.code.Count
        Dim lastInst As Integer = -1

        'put code to block 1 until address is reached
        While hasWork
            Dim tInst As codeBlock = b.code(fromIdx)
            If tInst.realAddress <= pStruct.rangeEnd Then
                lastInst = b1.code.Count
                b1.code.Add(tInst)
                If b1.code.Count = 1 Then
                    b1.realAddress = tInst.realAddress
                End If
                b.code.RemoveAt(fromIdx)
                If fromIdx >= b.code.Count Then
                    hasWork = False
                End If
            Else
                hasWork = False
            End If
        End While

        'check if last code is a forward jump
        'convert:
        '  branch to block2
        '    block1
        '    jump to end of block2
        '    block2
        'into:
        '  if 
        '    block1
        '  else
        '    block2
        If pStruct.hasElse And b1.code.Count > 0 And lastInst = b1.code.Count - 1 Then

            If Not b1.code(lastInst).isBlock Then
                Dim tInst2 As instruction = b1.code(lastInst)
                If tInst2.type = InstructionType.JUMP Then
                    b1.code.RemoveAt(lastInst)
                    'fill block 2
                    hasWork = True
                    While hasWork
                        Dim tInst As codeBlock = b.code(fromIdx)
                        If tInst.realAddress <= pStruct.elseRangeEnd Then
                            b2.code.Add(tInst)
                            If b2.code.Count = 1 Then
                                b2.realAddress = tInst.realAddress
                            End If
                            b.code.RemoveAt(fromIdx)
                            If fromIdx >= b.code.Count Then
                                hasWork = False
                            End If
                        Else
                            hasWork = False
                        End If
                    End While
                    newBlock.code.Add(b2)
                End If
            End If
        End If
        Return newBlock
    End Function


    Private Sub convertToIfBlock(pBlock As block, pStruct As structRange)
        Dim bIdx As Integer = findIndexInBlock(pBlock, pStruct.rangeStart, 0, pBlock.code.Count - 1)
        If bIdx > 0 Then
            If pBlock.code(bIdx).isBlock Then
                convertToIfBlock(pBlock.code(bIdx), pStruct)
            Else
                Dim b As instPBranch = pBlock.code(bIdx)
                Dim nBlock As block = extractIfBlock(pBlock, bIdx + 1, pStruct)
                'reverse the condition to become the if condition
                b.flagIsSet = Not b.flagIsSet
                nBlock.code.Insert(0, b)
                nBlock.realAddress = b.realAddress
                pBlock.code(bIdx) = nBlock
            End If
        End If
    End Sub

    Private Sub convertToLoopBlock(pBlock As block, pStruct As structRange)
        Dim bIdx As Integer = findIndexInBlock(pBlock, pStruct.rangeEnd, 0, pBlock.code.Count - 1)
        If bIdx > 0 Then
            If pBlock.code(bIdx).isBlock Then
                convertToLoopBlock(pBlock.code(bIdx), pStruct)
            Else
                Dim cIdx As Integer = findIndexInBlock(pBlock, pStruct.rangeStart, 0, pBlock.code.Count - 1)
                Dim tInst As instruction = pBlock.code(bIdx)
                pBlock.code.RemoveAt(bIdx)
                Dim nBlock As block = extractBranchLoopBlock(pBlock, bIdx - 1, pStruct.rangeStart)
                If tInst.type = InstructionType.JUMP Then
                    If Not CType(nBlock.code(0), block).code(0).isBlock Then
                        Dim tInst2 As instruction = CType(nBlock.code(0), block).code(0)
                        If tInst2.type = InstructionType.BRANCH Or tInst2.type = InstructionType.COMPARE_BRANCH Or tInst2.type = InstructionType.LOAD_BRANCH Then
                            Dim tBranch As instPBranch = tInst2
                            If tBranch.branchToAddress = tInst.nextAddress Then
                                'while loop
                                tBranch.flagIsSet = Not tBranch.flagIsSet
                                CType(nBlock.code(0), block).code.RemoveAt(0)
                                nBlock.code.Insert(0, tBranch)
                            End If
                        End If
                    End If

                End If
                'add loop inst
                nBlock.code.Add(tInst)
                'replace with block
                pBlock.code.Insert(cIdx, nBlock)
            End If
        End If
    End Sub

    Private Function findLoopInBlock(pBlock As block, pStruct As structRange) As block
        Dim bIdx As Integer = 0
        While bIdx < pBlock.code.Count
            If pBlock.code(bIdx).isBlock Then
                Dim tBlock As block = pBlock.code(bIdx)
                If tBlock.codeRange.rangeStart <= pStruct.rangeStart And tBlock.codeRange.rangeEnd >= pStruct.rangeEnd Then
                    If tBlock.type = BlockType.BRANCH_LOOP And tBlock.codeRange.rangeEnd = pStruct.rangeEnd Then
                        Return tBlock
                    Else
                        Return findLoopInBlock(tBlock, pStruct)
                    End If
                End If
            End If
            bIdx += 1
        End While
        Return Nothing
    End Function

    Private Sub detectBreakFromLoop(pBlock As block, pStruct As structRange)
        Dim hasExit As Boolean = False

        Dim bIdx As Integer = findIndexInBlock(pBlock, pStruct.rangeEnd, 0, pBlock.code.Count - 1)
        If bIdx > 0 Then
            If pBlock.code(bIdx).isBlock Then
                Dim tBlock As block = pBlock.code(bIdx)
                Dim structFound As Boolean = False
                If pBlock.type = BlockType.BRANCH_LOOP Then
                    If tBlock.codeRange.rangeEnd = pStruct.rangeEnd Then

                        structFound = True
                        Dim codeB As block
                        If tBlock.code(0).isBlock Then
                            codeB = tBlock.code(0)
                        Else
                            codeB = tBlock.code(1)
                        End If
                        Dim cIdx As Integer = 0
                        While cIdx < codeB.code.Count
                            If Not codeB.code(cIdx).isBlock Then
                                Dim tInst As instruction = codeB.code(cIdx)
                                If tInst.type = InstructionType.BRANCH Or tInst.type = InstructionType.COMPARE_BRANCH Or tInst.type = InstructionType.LOAD_BRANCH Then
                                    Dim bInst As instPBranch = tInst
                                    If bInst.branchToAddress = pStruct.nextAddress Then
                                        'found break from loop
                                        'convert to if block with break
                                        Dim ifBlock As New block
                                        ifBlock.type = BlockType.IF_THEN_ELSE
                                        'add condition
                                        ifBlock.code.Add(bInst)
                                        ifBlock.realAddress = bInst.realAddress
                                        'add if part
                                        Dim ifPart As New block
                                        ifPart.type = BlockType.IF_BLOCK
                                        ifBlock.code.Add(ifPart)
                                        'add jump to if part
                                        Dim newJump As New instBreakLoop
                                        newJump.opName = "BREAK"
                                        newJump.realAddress = bInst.realAddress + 1
                                        ifPart.addCodeBlock(newJump)
                                        ifPart.realAddress = newJump.realAddress
                                        codeB.code(cIdx) = ifBlock
                                        hasExit = True
                                    End If
                                End If
                            End If
                            cIdx += 1
                        End While
                    End If
                End If
                If Not structFound Then
                    detectBreakFromLoop(tBlock, pStruct)
                End If
            End If
        End If
    End Sub

    Private Function detectHasExitFromLoop(pBlock As block, pStruct As structRange) As Boolean
        Dim bIdx As Integer = 0
        While bIdx < pBlock.code.Count
            If pBlock.code(bIdx).isBlock Then
                Dim tBlock As block = pBlock.code(bIdx)
                If detectHasExitFromLoop(tBlock, pStruct) Then
                    Return True
                End If
            Else
                Dim tInst As instruction = pBlock.code(bIdx)
                Select Case tInst.type
                    Case InstructionType.BRANCH, InstructionType.COMPARE_BRANCH, InstructionType.LOAD_BRANCH
                        Dim bInst As instPBranch = tInst
                        If bInst.branchToAddress < pStruct.rangeStart Or bInst.branchToAddress > pStruct.rangeEnd Then
                            Return True
                        End If
                    Case InstructionType.JUMP
                        Dim jInst As instJump = tInst
                        If jInst.jumpToRealAddress < pStruct.rangeStart Or jInst.jumpToRealAddress > pStruct.rangeEnd Then
                            Return True
                        End If
                    Case InstructionType.SUB_RETURN
                        Return True
                End Select

            End If
            bIdx += 1
        End While
        Return False
    End Function


    Private Function findCallCnt(name As String, c As codeBlock) As Integer
        Dim cnt As Integer = 0
        If c.isBlock Then
            Dim tBlock As block = c
            For Each subC As codeBlock In tBlock.code
                cnt += findCallCnt(name, subC)
            Next
        Else
            Dim tInst As instruction = c
            If tInst.type = InstructionType.JUMP_BLOCK Then
                Dim jInst As instJumpBlock = tInst
                If jInst.blockName = name Then
                    cnt += 1
                End If
            End If
        End If
        Return cnt
    End Function

    Private Sub replaceCall(name As String, c As block, i As instruction)
        Dim cnt As Integer = 0
        For j As Integer = 0 To c.code.Count - 1
            If c.code(j).isBlock Then
                replaceCall(name, c.code(j), i)
            Else
                Dim inst As instruction = c.code(j)
                If inst.type = InstructionType.JUMP_BLOCK Then
                    Dim jInst As instJumpBlock = inst
                    If jInst.blockName = name Then
                        'replace with the instruction
                        c.code(j) = i
                    End If
                End If
            End If
        Next
    End Sub


    Private Function findJBLInBlock(targetAddress As String, b As block, ByRef resultB As block, ByRef i As Integer) As Boolean
        Dim found As Boolean = False
        For idx As Integer = 0 To b.code.Count - 1
            If b.code(i).isBlock Then
                Dim tBlock As block = b.code(i)
                Dim rBlock As block
                Dim rIdx As Integer
                If findJBLInBlock(targetAddress, tBlock, rBlock, rIdx) Then
                    resultB = rBlock
                    i = rIdx
                    Return True
                End If
            Else
                Dim tInst As instruction = b.code(i)
                If tInst.type = InstructionType.JUMP_BLOCK Then
                    Dim tJump As instJumpBlock = tInst
                    If tJump.blockName = targetAddress Then
                        resultB = b
                        i = idx
                        Return True
                    End If
                End If
            End If
        Next
        Return False
    End Function



    Private Function extractBranchLoopBlock(b As block, fromIdx As Integer, address As UInt32) As block
        Dim newBlock As New block
        newBlock.type = BlockType.BRANCH_LOOP

        Dim b1 As New block
        b1.type = BlockType.LOOP_CONTENT

        newBlock.addCodeBlock(b1)

        Dim hasWork As Boolean = True

        'put code to block 1 until address is reached
        While hasWork
            Dim tInst As codeBlock = b.code(fromIdx)
            If tInst.realAddress >= address Then
                b1.code.Insert(0, tInst)
                b1.realAddress = tInst.realAddress
                b.code.RemoveAt(fromIdx)
                fromIdx -= 1
                If fromIdx < 0 Then
                    hasWork = False
                End If
            Else
                hasWork = False
            End If
        End While
        newBlock.realAddress = b1.realAddress
        Return newBlock
    End Function

    Private Function splitBlock(b As block, address As UInt32) As block
        Dim newBlock As New block
        newBlock.type = BlockType.SUBROUNTINE

        Dim i As Integer = 0
        Dim tInst As codeBlock
        Dim hasMoved As Boolean
        Do Until i >= b.code.Count
            hasMoved = False
            tInst = b.code(i)
            If tInst.realAddress >= address Then
                If newBlock.code.Count = 0 Then
                    newBlock.realAddress = tInst.realAddress
                End If
                newBlock.addCodeBlock(tInst)
                b.code.RemoveAt(i)
                hasMoved = True
            End If

            If Not hasMoved Then
                i += 1
            End If
        Loop
        If newBlock.code.Count > 0 Then
            newBlock.name = "SUB_" & address.ToString("X6")
        End If
        getLastInst(b).nextAddress = UInt32.MaxValue

        Return newBlock
    End Function


    Private Function getLastInst(b As block) As instruction
        Dim tBlock As block
        Dim tI As instruction
        tBlock = b
        While tBlock.code(tBlock.code.Count - 1).isBlock
            tBlock = tBlock.code(tBlock.code.Count - 1)
        End While
        tI = tBlock.code(tBlock.code.Count - 1)
        Return tI
    End Function

    Private Function createIfJumpBlock(bInst As instPBranch, type As JumpBlockType) As block
        Dim ifBlock As New block
        ifBlock.type = BlockType.IF_THEN_ELSE
        'add condition
        ifBlock.code.Add(bInst)
        ifBlock.realAddress = bInst.realAddress
        'add if part
        Dim ifPart As New block
        ifPart.type = BlockType.IF_BLOCK
        ifBlock.code.Add(ifPart)
        'add jump to if part
        Dim newJump As New instJumpBlock
        newJump = New instJumpBlock
        newJump.jumpType = type
        newJump.opName = "JBL"
        newJump.realAddress = bInst.realAddress + 1
        If type = JumpBlockType.JGT Then
            newJump.blockName = "L_" & bInst.branchToAddress.ToString("X6")
        Else
            newJump.blockName = "SUB_" & bInst.branchToAddress.ToString("X6")
        End If
        ifPart.addCodeBlock(newJump)
        ifPart.realAddress = newJump.realAddress
        Return ifBlock
    End Function

    'Private Function traceMemoryValue(memID As memoryID, atAddress As UInt32) As List(Of addressRange)
    '    Dim startP As Integer = findInstructionIndex(atAddress, 0, fullCode.Count - 1)
    '    Dim active As addressRange = getActiveSection(atAddress)
    '    Dim result As New List(Of addressRange)
    '    Dim currentResults As New List(Of valueTraceResult)

    '    Dim p As Integer = startP - 1
    '    'move back to find index is first set
    '    Dim instFound As Boolean = False
    '    Dim traceEnd As Boolean = False
    '    Dim checkedAddress As UInt32 = atAddress
    '    Dim valueSource As memoryTarget

    '    Do Until instFound Or fullCode(p).realAddress < active.rangeStart
    '        Dim tList As List(Of memoryTarget) = fullCode(p).getOverwrittenMemoryTarget()
    '        checkedAddress = fullCode(p).realAddress
    '        For Each m As memoryTarget In tList
    '            If m.realAddress.Type = memID.Type And m.realAddress.ID = memID.ID Then
    '                Select Case fullCode(p).type
    '                    Case InstructionType.TRANSFER
    '                        instFound = True
    '                        Dim b As instTransfer = CType(fullCode(p), instTransfer)
    '                        valueSource = b.source
    '                        Dim tmp As New valueTraceResult
    '                        tmp.valueFor = memID
    '                        tmp.valueValidAt = fullCode(p).realAddress

    '                        If b.source.addrMode = AddressingMode.IMMEDIATE Then
    '                            Dim range As addressRange
    '                            range.rangeStart = b.source.realAddress.ID
    '                            range.rangeEnd = b.source.realAddress.ID
    '                            tmp.addRange(range)
    '                        ElseIf b.source.addrMode = AddressingMode.ABSOLUTE And b.source.realAddress.Type = MemoryType.PRG_ROM Then
    '                            Dim valueByte As Byte = prgROM(b.source.realAddress.ID)
    '                            Dim range As addressRange
    '                            range.rangeStart = valueByte
    '                            range.rangeEnd = valueByte
    '                            tmp.addRange(range)
    '                        ElseIf b.source.addrMode = AddressingMode.ABSOLUTE And b.source.realAddress.Type <> MemoryType.RAM Then
    '                            'hardware address
    '                            Dim range As addressRange
    '                            range.rangeStart = 0
    '                            range.rangeEnd = &HFF
    '                            tmp.addRange(range)
    '                        Else
    '                            'if set by transfer, trace the source
    '                            Dim tmpResult As List(Of addressRange) = traceMemoryValue(b.source.realAddress, fullCode(p).realAddress)
    '                            For Each r As addressRange In tmpResult
    '                                tmp.addRange(r)
    '                            Next
    '                        End If
    '                        If tmp.values.Count > 0 Then
    '                            currentResults.Add(tmp)
    '                        End If
    '                End Select
    '            End If
    '        Next
    '        p -= 1
    '    Loop
    '    'find jumps to this section and trace from those jumps
    '    For Each j As addressRange In jumpLinks
    '        If j.rangeEnd >= checkedAddress And j.rangeEnd <= atAddress Then
    '            Dim tmp As New valueTraceResult
    '            tmp.valueFor = memID
    '            tmp.valueValidAt = j.rangeEnd
    '            Dim tmpResult As List(Of addressRange) = traceMemoryValue(memID, j.rangeStart)
    '            For Each r As addressRange In tmpResult
    '                tmp.addRange(r)
    '            Next
    '            If tmp.values.Count > 0 Then
    '                currentResults.Add(tmp)
    '            End If
    '        End If
    '    Next


    '    'find points where the value is set
    '    'go backwards to find the first point where the value is set
    '    'check for branches that jumps to anywhere between the two points
    '    'trace backwards through the branches
    '    'beware of loops
    '    'for each point, see if the value assigned is constant
    '    'if not, trace the assigned value from that point
    '    'handle modifiations to the value
    'End Function

    'Private Function getActiveSection(address As UInt32) As addressRange
    '    Dim r As addressRange
    '    r.rangeStart = address
    '    r.rangeEnd = address
    '    For Each t As traceTask In traceTasksToRun
    '        For Each s As addressRange In t.sectionList
    '            If address >= s.rangeStart And address <= s.rangeEnd Then
    '                If r.rangeStart > s.rangeStart Then
    '                    r.rangeStart = s.rangeStart
    '                End If
    '                If r.rangeEnd < s.rangeEnd Then
    '                    r.rangeEnd = s.rangeEnd
    '                End If
    '            End If
    '        Next
    '    Next
    '    Return r
    'End Function


    Public Sub generate()
        Dim s As String = ""
        For Each bl As block In blocks
            s &= bl.printToHeader
        Next
        frm.txtCHeader.Text = s

        s = ""
        For Each bl As block In blocks
            s &= bl.printToCode("")
        Next
        frm.txtCCode.Text = s
    End Sub
End Module
