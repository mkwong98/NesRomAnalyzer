Imports System.CodeDom
Imports System.ComponentModel
Imports System.Net
Imports System.Reflection.Emit

Public Enum AddressingMode
    IMPLICIT
    ACCUMULATOR
    IMMEDIATE
    ZERO_PAGE
    ABSOLUTE
    RELATIVE
    INDIRECT
    ZERO_PAGE_INDEXED_X
    ZERO_PAGE_INDEXED_Y
    ABSOLUTE_INDEXED_X
    ABSOLUTE_INDEXED_Y
    INDEXED_INDIRECT_X
    INDIRECT_INDEXED_Y
End Enum

Public Enum CpuRegister
    a
    x
    y
    pc
    s
    p
End Enum

Public Enum FlagID
    c
    z
    i
    d
    v
    n
End Enum

Structure opInfo
    Public name As String
    Public length As Byte
    Public mode As AddressingMode
End Structure

Structure cpuFlag
    Public value As Boolean
    Public source As memoryID
End Structure

Structure branchEvent
    Public sourceID As UInt32
    Public target As UInt16
End Structure

Module cpu
    Private a As memoryByte
    Private x As memoryByte
    Private y As memoryByte
    Private pc(1) As memoryByte
    Private stackPointer As memoryByte
    Private flgCarry As cpuFlag
    Private flgZero As cpuFlag
    Private flgIntrDis As cpuFlag
    Private flgDecimal As cpuFlag
    Private flgOverflow As cpuFlag
    Private flgNegative As cpuFlag

    Public opTable(255) As opInfo

    Private branchAddress As List(Of branchEvent)
    Public indirectJmpList As List(Of instJump)


    Public Sub init()
        initOpTable()

        'assume stackpointer always changes
        stackPointer.unchanged = False
        stackPointer.source.Type = MemoryType.CPU_REG
        stackPointer.source.ID = CpuRegister.s

        branchAddress = New List(Of branchEvent)
        indirectJmpList = New List(Of instJump)

    End Sub

    Public Sub powerOn()
        Dim s As memoryID
        s.Type = MemoryType.INIT
        s.ID = 0
        a.currentValue = 0
        a.source = s
        x.currentValue = 0
        x.source = s
        y.currentValue = 0
        y.source = s
        stackPointer.currentValue = &HFD
        writeStatusReg(&H4, s)
    End Sub

    Private Function readStatusReg() As Byte
        Dim v As Byte = 0
        If flgCarry.value Then v = v Or bitMask(0)
        If flgZero.value Then v = v Or bitMask(1)
        If flgIntrDis.value Then v = v Or bitMask(2)
        If flgDecimal.value Then v = v Or bitMask(3)
        If flgOverflow.value Then v = v Or bitMask(6)
        If flgNegative.value Then v = v Or bitMask(7)
        Return v
    End Function

    Private Sub writeStatusReg(v As Byte, s As memoryID)
        flgCarry.value = ((v And bitMask(0)) <> 0)
        flgCarry.source = s
        flgZero.value = ((v And bitMask(1)) <> 0)
        flgZero.source = s
        flgIntrDis.value = ((v And bitMask(2)) <> 0)
        flgIntrDis.source = s
        flgDecimal.value = ((v And bitMask(3)) <> 0)
        flgDecimal.source = s
        flgOverflow.value = ((v And bitMask(6)) <> 0)
        flgOverflow.source = s
        flgNegative.value = ((v And bitMask(7)) <> 0)
        flgNegative.source = s
    End Sub

    Public Sub setupForTask(t As taskToRun)
        pc(0).currentValue = t.address And &HFF
        pc(1).currentValue = t.address >> 8
    End Sub

    Public Sub run()
        Dim isRunning As Boolean = True
        'read instruction
        Dim opCode As memoryByte
        Dim pcVal As UInt16
        While isRunning
            pcVal = CInt(pc(1).currentValue) << 8 Or pc(0).currentValue
            opCode = read(pcVal, PrgByteType.CODE_HEAD, 0)
            incPC()
            isRunning = runOpCode(opCode, pcVal)

            While branchAddress.Count > 0 And Not isRunning
                'handle possible branch that missed
                Dim b As branchEvent = branchAddress(0)
                branchAddress.RemoveAt(0)
                opCode = read(b.target, PrgByteType.PEEK, 0)
                If Not opCode.known Then
                    isRunning = True
                    pc(1).currentValue = b.target >> 8
                    pc(0).currentValue = b.target And &HFF

                    logBranch(b.target, b.sourceID)
                End If
            End While
        End While
    End Sub

    Private Function runOpCode(pOpCode As memoryByte, pAddress As UInt16) As Boolean
        Dim stillRunning As Boolean = True
        'get code len
        Dim l As Byte = getOpLen(pOpCode.currentValue)
        Dim i As Integer = 1
        Dim operand(1) As memoryByte
        While i < l
            operand(i - 1) = read(CInt(pc(1).currentValue) << 8 Or pc(0).currentValue, PrgByteType.CODE_HEAD, 0)
            incPC()
            i += 1
        End While

        If pOpCode.known And pOpCode.currentUsage = PrgByteType.CODE_HEAD Then
            Return False
        End If

        Dim tRemarks As String = ""
        Dim tAddress As UInt16
        Dim tMemory As memoryByte
        Dim oInst As instruction

        Select Case opTable(pOpCode.currentValue).name
            Case "RTI"
                Dim tInst As New instSubReturn
                tInst.restoreFlags = True
                oInst = tInst
                stillRunning = False
            Case "RTS"
                Dim tInst As New instSubReturn
                tInst.restoreFlags = False
                oInst = tInst
                stillRunning = False
            Case "JSR"
                tAddress = CInt(operand(1).currentValue) << 8 Or operand(0).currentValue
                tMemory = read(tAddress, PrgByteType.PEEK, 0)
                addJSRTask(tAddress, tMemory.source.ID)
                tRemarks = realAddressToHexStr(tMemory.source.ID)

                Dim tInst As New instSubroutine
                tInst.restoreFlags = False
                tInst.subAddress = tAddress
                tInst.subRealAddress = tMemory.source.ID
                oInst = tInst
            Case "JMP"
                tAddress = CInt(operand(1).currentValue) << 8 Or operand(0).currentValue
                tMemory = read(tAddress, PrgByteType.PEEK, 0)

                Dim tInst As New instJump
                tInst.jumpToAddress = tAddress
                tInst.jumpToRealAddress = tMemory.source.ID
                If opTable(pOpCode.currentValue).mode = AddressingMode.ABSOLUTE Then
                    If tMemory.known Then
                        stillRunning = False
                    Else
                        pc(1).currentValue = operand(1).currentValue
                        pc(0).currentValue = operand(0).currentValue
                    End If
                    tRemarks = realAddressToHexStr(tMemory.source.ID)
                    tInst.isIndirect = False
                Else
                    tInst.isIndirect = True
                    tInst.jumpToAddress = tAddress
                    'unknown jump target
                    'add to lsv
                    indirectJmpList.Add(tInst)
                    stillRunning = False
                End If
                oInst = tInst

            Case "BRK"
                addBRKTask(pOpCode.source)

                Dim tInst As New instSubroutine
                tInst.restoreFlags = True
                tInst.subAddress = readAsAddress(&HFFFE, PrgByteType.PEEK, 0)
                tInst.subRealAddress = read(tInst.subAddress, PrgByteType.PEEK, 0).source.ID
                oInst = tInst
                incPC()

            Case "LDA"
                Dim tInst As New instTransfer
                a = handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.source)
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "LDX"
                Dim tInst As New instTransfer
                x = handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.source)
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "LDY"
                Dim tInst As New instTransfer
                y = handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.source)
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.y)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "STA"
                Dim tInst As New instTransfer
                handleWriteAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, a, pOpCode.source, tRemarks, tInst.destination)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                oInst = tInst
            Case "STX"
                Dim tInst As New instTransfer
                handleWriteAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, x, pOpCode.source, tRemarks, tInst.destination)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                oInst = tInst
            Case "STY"
                Dim tInst As New instTransfer
                handleWriteAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, y, pOpCode.source, tRemarks, tInst.destination)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.y)
                oInst = tInst
            Case "TAX"
                Dim tInst As New instTransfer
                x = a
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "TAY"
                Dim tInst As New instTransfer
                y = a
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.y)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "TSX"
                Dim tInst As New instTransfer
                'assume stackpointer always changes
                x = stackPointer
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.s)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "TXA"
                Dim tInst As New instTransfer
                a = x
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "TXS"
                Dim tInst As New instTransfer
                'assume stackpointer always changes
                stackPointer.currentValue = x.currentValue
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.s)
                oInst = tInst
            Case "TYA"
                Dim tInst As New instTransfer
                a = y
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.source = codeBlock.createCPURegisterMemoryTarget(CpuRegister.y)
                tInst.destination = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "ADC", "SBC"
                Dim tInst As New instAccModify
                'do not update the value
                handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand)
                a.unchanged = False
                a.source = pOpCode.source
                handleFlagUpdate(pOpCode.source, True, True, False, False, True, True)
                tInst.requireFlagC = True
                tInst.setFlagChange(True, True, False, False, True, True)
                oInst = tInst
            Case "INC", "DEC"
                Dim tInst As New instModify
                tMemory = handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand)
                tMemory.unchanged = False
                tMemory.source = pOpCode.source
                handleWriteAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, tMemory, pOpCode.source, tRemarks, tInst.operand)
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "INX", "DEX"
                Dim tInst As New instModify
                x.unchanged = False
                x.source = pOpCode.source
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.operand = codeBlock.createCPURegisterMemoryTarget(CpuRegister.x)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "INY", "DEY"
                Dim tInst As New instModify
                y.unchanged = False
                y.source = pOpCode.source
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.operand = codeBlock.createCPURegisterMemoryTarget(CpuRegister.y)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "ASL", "LSR", "ROL", "ROR"
                Dim tInst As New instModify
                If opTable(pOpCode.currentValue).mode = AddressingMode.ACCUMULATOR Then
                    a.unchanged = False
                    a.source = pOpCode.source
                    tInst.operand = codeBlock.createCPURegisterMemoryTarget(CpuRegister.a)
                Else
                    tMemory = handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand)
                    tMemory.unchanged = False
                    tMemory.source = pOpCode.source
                    handleWriteAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, tMemory, pOpCode.source, tRemarks, tInst.operand)
                End If
                handleFlagUpdate(pOpCode.source, True, True, False, False, False, True)
                tInst.setFlagChange(True, True, False, False, False, True)
                If opTable(pOpCode.currentValue).name = "ROL" Or opTable(pOpCode.currentValue).name = "ROR" Then
                    tInst.requireFlagC = True
                End If
                oInst = tInst
            Case "AND", "ORA", "EOR"
                Dim tInst As New instAccModify
                'do not update the value
                handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand)
                a.unchanged = False
                a.source = pOpCode.source
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "BIT"
                Dim tInst As New instAccModify
                handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand)
                handleFlagUpdate(pOpCode.source, False, True, False, False, True, True)
                tInst.setFlagChange(False, True, False, False, True, True)
                oInst = tInst
            Case "CMP", "CPX", "CPY"
                Dim tInst As New instCompare
                Select Case opTable(pOpCode.currentValue).name
                    Case "CMP"
                        tInst.operand1 = CpuRegister.a
                    Case "CPX"
                        tInst.operand1 = CpuRegister.x
                    Case "CPY"
                        tInst.operand1 = CpuRegister.y
                End Select
                handleReadAddressMode(opTable(pOpCode.currentValue).mode, operand(0).currentValue, operand(1).currentValue, pOpCode.source, tRemarks, tInst.operand2)
                handleFlagUpdate(pOpCode.source, True, True, False, False, False, True)
                tInst.setFlagChange(True, True, False, False, False, True)
                oInst = tInst

            Case "BCC", "BCS", "BEQ", "BNE", "BPL", "BMI", "BVC", "BVS"
                Dim tInst As New instBranch
                Select Case opTable(pOpCode.currentValue).name
                    Case "BCC"
                        tInst.useFlag = FlagID.c
                        tInst.flagIsSet = False
                    Case "BCS"
                        tInst.useFlag = FlagID.c
                        tInst.flagIsSet = True
                    Case "BEQ"
                        tInst.useFlag = FlagID.z
                        tInst.flagIsSet = True
                    Case "BNE"
                        tInst.useFlag = FlagID.z
                        tInst.flagIsSet = False
                    Case "BPL"
                        tInst.useFlag = FlagID.n
                        tInst.flagIsSet = False
                    Case "BMI"
                        tInst.useFlag = FlagID.n
                        tInst.flagIsSet = True
                    Case "BVC"
                        tInst.useFlag = FlagID.v
                        tInst.flagIsSet = False
                    Case "BVS"
                        tInst.useFlag = FlagID.v
                        tInst.flagIsSet = True
                End Select
                'calculate branch address
                tAddress = CInt(pc(1).currentValue) << 8 Or pc(0).currentValue
                If operand(0).currentValue > &H7F Then
                    tAddress -= &H100
                End If
                tAddress += operand(0).currentValue
                tMemory = read(tAddress, PrgByteType.PEEK, 0)
                If Not tMemory.known Then
                    Dim b As branchEvent
                    b.sourceID = pOpCode.source.ID
                    b.target = tAddress
                    branchAddress.Add(b)
                End If
                tRemarks = realAddressToHexStr(tMemory.source.ID)
                tInst.branchToAddress = tMemory.source.ID
                oInst = tInst
            Case "PHA"
                Dim tInst As New instStack
                tInst.regToKeep = CpuRegister.a
                tInst.isPush = True
                oInst = tInst
            Case "PLA"
                Dim tInst As New instStack
                a.unchanged = False
                a.source = pOpCode.source
                tInst.regToKeep = CpuRegister.a
                tInst.isPush = False
                handleFlagUpdate(pOpCode.source, False, True, False, False, False, True)
                tInst.setFlagChange(False, True, False, False, False, True)
                oInst = tInst
            Case "PHP"
                Dim tInst As New instStack
                tInst.regToKeep = CpuRegister.p
                tInst.isPush = True
                oInst = tInst
            Case "PLP"
                Dim tInst As New instStack
                tInst.regToKeep = CpuRegister.p
                tInst.isPush = False
                handleFlagUpdate(pOpCode.source, True, True, True, True, True, True)
                tInst.setFlagChange(True, True, True, True, True, True)
                oInst = tInst
            Case "CLC", "SEC"
                Dim tInst As New instFlag
                tInst.setUpdateFlag(FlagID.c)
                tInst.isClear = opTable(pOpCode.currentValue).name = "CLC"
                handleFlagUpdate(pOpCode.source, True, False, False, False, False, False)
                tInst.setFlagChange(True, False, False, False, False, False)
                oInst = tInst
            Case "CLI", "SEI"
                If opTable(pOpCode.currentValue).name = "SEI" Then
                    addBRKTask(pOpCode.source)
                End If

                Dim tInst As New instFlag
                tInst.setUpdateFlag(FlagID.i)
                tInst.isClear = opTable(pOpCode.currentValue).name = "CLI"
                handleFlagUpdate(pOpCode.source, False, False, True, False, False, False)
                tInst.setFlagChange(False, False, True, False, False, False)
                oInst = tInst
            Case "CLD", "SED"
                Dim tInst As New instFlag
                tInst.setUpdateFlag(FlagID.d)
                tInst.isClear = opTable(pOpCode.currentValue).name = "CLD"
                handleFlagUpdate(pOpCode.source, False, False, False, True, False, False)
                tInst.setFlagChange(False, False, False, True, False, False)
                oInst = tInst
            Case "CLV"
                Dim tInst As New instFlag
                tInst.setUpdateFlag(FlagID.v)
                tInst.isClear = True
                handleFlagUpdate(pOpCode.source, False, False, False, False, True, False)
                tInst.setFlagChange(False, False, False, False, True, False)
                oInst = tInst
            Case "NOP"
                Dim tInst As New instNOP
                oInst = tInst
            Case Else
                Dim tInst As New instNOP
                oInst = tInst
                stillRunning = False
        End Select
        oInst.realAddress = pOpCode.source.ID
        oInst.opName = opTable(pOpCode.currentValue).name


        logLineOfCode(pAddress, pOpCode.source.ID, pOpCode.currentValue, operand(0).currentValue, operand(1).currentValue, tRemarks, oInst)

        'If ((CInt(pc(1).currentValue) << 8 Or pc(0).currentValue) - &H8000 > getPrgROMSize()) _
        '    Or (CInt(pc(1).currentValue) << 8 Or pc(0).currentValue) < &H8000 Then
        '    'pc is out of range, stop execution
        '    stillRunning = False
        'End If
        Return stillRunning
    End Function

    Private Function handleReadAddressMode(m As AddressingMode, op0 As Byte, op1 As Byte, pSource As memoryID, ByRef remarks As String, ByRef t As memoryTarget) As memoryByte
        Dim v As memoryByte
        t.addrMode = m
        Select Case m
            Case AddressingMode.IMMEDIATE
                v.source = pSource
                v.unchanged = True
                v.currentValue = op0
                t.address = op0
                t.realAddress.Type = MemoryType.INIT
                t.realAddress.ID = op0
                Return v
            Case AddressingMode.ACCUMULATOR
                t.address = CpuRegister.a
                t.realAddress.Type = MemoryType.CPU_REG
                t.realAddress.ID = CpuRegister.a
                Return a
            Case AddressingMode.ZERO_PAGE
                v = read(op0, PrgByteType.DATA, pSource.ID)
                v.unchanged = False
                remarks = getMemoryName(v.source)
                t.address = op0
                t.realAddress = v.source
                Return v
            Case AddressingMode.ZERO_PAGE_INDEXED_X
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+X"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.ZERO_PAGE_INDEXED_Y
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+Y"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.ABSOLUTE
                t.address = CInt(op1) << 8 Or op0
                v = read(t.address, PrgByteType.DATA, pSource.ID)
                v.unchanged = False
                remarks = getMemoryName(v.source)
                t.realAddress = v.source
                Return v
            Case AddressingMode.ABSOLUTE_INDEXED_X
                t.address = CInt(op1) << 8 Or op0
                v = read(CInt(op1) << 8 Or op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+X"
                t.realAddress = v.source
            Case AddressingMode.ABSOLUTE_INDEXED_Y
                t.address = CInt(op1) << 8 Or op0
                v = read(CInt(op1) << 8 Or op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+Y"
                t.realAddress = v.source
            Case AddressingMode.INDEXED_INDIRECT_X
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = "(RAM(" & addressToHexStr(op0) & "+X))"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.INDIRECT_INDEXED_Y
                v = read(op0, PrgByteType.DATA, pSource.ID)
                remarks = "(RAM(" & addressToHexStr(op0) & ")+Y)"
                t.address = op0
                t.realAddress = v.source
        End Select
        v.source.Type = MemoryType.RANDOM
        v.unchanged = False
        Return v
    End Function


    Private Sub handleWriteAddressMode(m As AddressingMode, op0 As Byte, op1 As Byte, pV As memoryByte, pSource As memoryID, ByRef remarks As String, ByRef t As memoryTarget)
        Dim v As memoryByte
        t.addrMode = m
        Select Case m
            Case AddressingMode.ZERO_PAGE
                write(op0, pV)
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source)
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.ZERO_PAGE_INDEXED_X
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+X"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.ZERO_PAGE_INDEXED_Y
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+Y"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.ABSOLUTE
                t.address = CInt(op1) << 8 Or op0
                write(t.address, pV)
                v = read(t.address, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source)
                t.realAddress = v.source
            Case AddressingMode.ABSOLUTE_INDEXED_X
                t.address = CInt(op1) << 8 Or op0
                v = read(t.address, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+X"
                t.realAddress = v.source
            Case AddressingMode.ABSOLUTE_INDEXED_Y
                t.address = CInt(op1) << 8 Or op0
                v = read(t.address, PrgByteType.PEEK, pSource.ID)
                remarks = getMemoryName(v.source) & "+Y"
                t.realAddress = v.source
            Case AddressingMode.INDEXED_INDIRECT_X
                v = read(op0, PrgByteType.PEEK, pSource.ID)
                remarks = "(RAM(" & addressToHexStr(op0) & "+X))"
                t.address = op0
                t.realAddress = v.source
            Case AddressingMode.INDIRECT_INDEXED_Y
                v = read(op0, PrgByteType.DATA, pSource.ID)
                remarks = "(RAM(" & addressToHexStr(op0) & ")+Y)"
                t.address = op0
                t.realAddress = v.source
        End Select

    End Sub

    Private Sub handleFlagUpdate(s As memoryID, c As Boolean, z As Boolean, i As Boolean, d As Boolean, v As Boolean, n As Boolean)
        If c Then flgCarry.source = s
        If z Then flgZero.source = s
        If i Then flgIntrDis.source = s
        If d Then flgDecimal.source = s
        If v Then flgOverflow.source = s
        If n Then flgNegative.source = s
    End Sub


    Private Sub incPC()
        If pc(0).currentValue = &HFF Then
            pc(0).currentValue = 0
            pc(1).currentValue += 1
        Else
            pc(0).currentValue += 1
        End If
    End Sub


    Public Function getOpLen(pOpCode As Byte) As Byte
        Return opTable(pOpCode).length
    End Function

    Private Sub initOpTable()
        'fill the table
        For i As UInt16 = 0 To 255
            opTable(i).length = 0
        Next
        'ADC Add with carry
        fillOpInfo(opTable(&H69), "ADC", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&H65), "ADC", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H75), "ADC", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H61), "ADC", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&H71), "ADC", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&H6D), "ADC", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H7D), "ADC", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&H79), "ADC", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'AND Bitwise AND
        fillOpInfo(opTable(&H29), "AND", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&H25), "AND", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H35), "AND", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H21), "AND", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&H31), "AND", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&H2D), "AND", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H3D), "AND", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&H39), "AND", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'ASL Arithmetic shift left
        fillOpInfo(opTable(&HA), "ASL", 1, AddressingMode.ACCUMULATOR)
        fillOpInfo(opTable(&H6), "ASL", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H16), "ASL", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HE), "ASL", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H1E), "ASL", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'BCC Branch if carry clear
        fillOpInfo(opTable(&H90), "BCC", 2, AddressingMode.RELATIVE)

        'BCS Branch if carry set
        fillOpInfo(opTable(&HB0), "BCS", 2, AddressingMode.RELATIVE)

        'BEQ Branch if equal
        fillOpInfo(opTable(&HF0), "BEQ", 2, AddressingMode.RELATIVE)

        'BIT Bit test
        fillOpInfo(opTable(&H24), "BIT", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H2C), "BIT", 3, AddressingMode.ABSOLUTE)

        'BMI Branch if minus
        fillOpInfo(opTable(&H30), "BMI", 2, AddressingMode.RELATIVE)

        'BNE Branch if not equal
        fillOpInfo(opTable(&HD0), "BNE", 2, AddressingMode.RELATIVE)

        'BPL Branch if plus
        fillOpInfo(opTable(&H10), "BPL", 2, AddressingMode.RELATIVE)

        'BRK Break
        fillOpInfo(opTable(&H0), "BRK", 1, AddressingMode.IMMEDIATE)

        'BVC Branch if overflow clear
        fillOpInfo(opTable(&H50), "BVC", 2, AddressingMode.RELATIVE)

        'BVS Branch if overflow set
        fillOpInfo(opTable(&H70), "BVS", 2, AddressingMode.RELATIVE)

        'CLC Clear carry
        fillOpInfo(opTable(&H18), "CLC", 1, AddressingMode.IMPLICIT)

        'CLD Clear decimal
        fillOpInfo(opTable(&HD8), "CLD", 1, AddressingMode.IMPLICIT)

        'CLI Clear interrupt disable
        fillOpInfo(opTable(&H58), "CLI", 1, AddressingMode.IMPLICIT)

        'CLV Clear overflow
        fillOpInfo(opTable(&HB8), "CLV", 1, AddressingMode.IMPLICIT)

        'CMP Compare A
        fillOpInfo(opTable(&HC9), "CMP", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HC5), "CMP", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HD5), "CMP", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HC1), "CMP", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&HD1), "CMP", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&HCD), "CMP", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HDD), "CMP", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&HD9), "CMP", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'CPX Compare X
        fillOpInfo(opTable(&HE0), "CPX", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HE4), "CPX", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HEC), "CPX", 3, AddressingMode.ABSOLUTE)

        'CPY Compare Y
        fillOpInfo(opTable(&HC0), "CPY", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HC4), "CPY", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HCC), "CPY", 3, AddressingMode.ABSOLUTE)

        'DEC Decrement memory
        fillOpInfo(opTable(&HC6), "DEC", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HD6), "DEC", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HCE), "DEC", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HDE), "DEC", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'DEX Decrement X
        fillOpInfo(opTable(&HCA), "DEX", 1, AddressingMode.IMPLICIT)

        'DEY Decrement Y
        fillOpInfo(opTable(&H88), "DEY", 1, AddressingMode.IMPLICIT)

        'EOR Bitwise Exclusive OR
        fillOpInfo(opTable(&H49), "EOR", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&H45), "EOR", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H55), "EOR", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H41), "EOR", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&H51), "EOR", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&H4D), "EOR", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H5D), "EOR", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&H59), "EOR", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'INC Increment Memory
        fillOpInfo(opTable(&HE6), "INC", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HF6), "INC", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HEE), "INC", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HFE), "INC", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'INX Increment X
        fillOpInfo(opTable(&HE8), "INX", 1, AddressingMode.IMPLICIT)

        'INY Increment Y
        fillOpInfo(opTable(&HC8), "INY", 1, AddressingMode.IMPLICIT)

        'JMP Jump
        fillOpInfo(opTable(&H4C), "JMP", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H6C), "JMP", 3, AddressingMode.INDIRECT)

        'JSR Jump to subroutine
        fillOpInfo(opTable(&H20), "JSR", 3, AddressingMode.ABSOLUTE)

        'LDA Load A
        fillOpInfo(opTable(&HA9), "LDA", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HA5), "LDA", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HB5), "LDA", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HA1), "LDA", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&HB1), "LDA", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&HAD), "LDA", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HBD), "LDA", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&HB9), "LDA", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'LDX Load X
        fillOpInfo(opTable(&HA2), "LDX", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HA6), "LDX", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HB6), "LDX", 2, AddressingMode.ZERO_PAGE_INDEXED_Y)
        fillOpInfo(opTable(&HAE), "LDX", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HBE), "LDX", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'LDY Load Y
        fillOpInfo(opTable(&HA0), "LDY", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HA4), "LDY", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HB4), "LDY", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HAC), "LDY", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HBC), "LDY", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'LSR Logical shift right
        fillOpInfo(opTable(&H4A), "LSR", 1, AddressingMode.ACCUMULATOR)
        fillOpInfo(opTable(&H46), "LSR", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H56), "LSR", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H4E), "LSR", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H5E), "LSR", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'NOP No operation
        fillOpInfo(opTable(&HEA), "NOP", 1, AddressingMode.IMPLICIT)

        'ORA Bitwise OR
        fillOpInfo(opTable(&H9), "ORA", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&H5), "ORA", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H15), "ORA", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H1), "ORA", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&H11), "ORA", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&HD), "ORA", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H1D), "ORA", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&H19), "ORA", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'PHA Push A
        fillOpInfo(opTable(&H48), "PHA", 1, AddressingMode.IMPLICIT)

        'PHP Push processor status
        fillOpInfo(opTable(&H8), "PHP", 1, AddressingMode.IMPLICIT)

        'PLA Pull A
        fillOpInfo(opTable(&H68), "PLA", 1, AddressingMode.IMPLICIT)

        'PLP Pull processor status
        fillOpInfo(opTable(&H28), "PLP", 1, AddressingMode.IMPLICIT)

        'ROL Rotate Left
        fillOpInfo(opTable(&H2A), "ROL", 1, AddressingMode.ACCUMULATOR)
        fillOpInfo(opTable(&H26), "ROL", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H36), "ROL", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H2E), "ROL", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H3E), "ROL", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'ROR Rotate right
        fillOpInfo(opTable(&H6A), "ROR", 1, AddressingMode.ACCUMULATOR)
        fillOpInfo(opTable(&H66), "ROR", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H76), "ROR", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H6E), "ROR", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H7E), "ROR", 3, AddressingMode.ABSOLUTE_INDEXED_X)

        'RTI Return from interrupt
        fillOpInfo(opTable(&H40), "RTI", 1, AddressingMode.IMPLICIT)

        'RTS Return from subroutine
        fillOpInfo(opTable(&H60), "RTS", 1, AddressingMode.IMPLICIT)

        'SBC Subtract with carry
        fillOpInfo(opTable(&HE9), "SBC", 2, AddressingMode.IMMEDIATE)
        fillOpInfo(opTable(&HE5), "SBC", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&HF5), "SBC", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&HE1), "SBC", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&HF1), "SBC", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&HED), "SBC", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&HFD), "SBC", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&HF9), "SBC", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'SEC Set carry
        fillOpInfo(opTable(&H38), "SEC", 1, AddressingMode.IMPLICIT)

        'SED Set decimal
        fillOpInfo(opTable(&HF8), "SED", 1, AddressingMode.IMPLICIT)

        'SEI Set interrupt disable
        fillOpInfo(opTable(&H78), "SEI", 1, AddressingMode.IMPLICIT)

        'STA Store A
        fillOpInfo(opTable(&H85), "STA", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H95), "STA", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H81), "STA", 2, AddressingMode.INDEXED_INDIRECT_X)
        fillOpInfo(opTable(&H91), "STA", 2, AddressingMode.INDIRECT_INDEXED_Y)
        fillOpInfo(opTable(&H8D), "STA", 3, AddressingMode.ABSOLUTE)
        fillOpInfo(opTable(&H9D), "STA", 3, AddressingMode.ABSOLUTE_INDEXED_X)
        fillOpInfo(opTable(&H99), "STA", 3, AddressingMode.ABSOLUTE_INDEXED_Y)

        'STX Store X
        fillOpInfo(opTable(&H86), "STX", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H96), "STX", 2, AddressingMode.ZERO_PAGE_INDEXED_Y)
        fillOpInfo(opTable(&H8E), "STX", 3, AddressingMode.ABSOLUTE)

        'STY Store Y
        fillOpInfo(opTable(&H84), "STY", 2, AddressingMode.ZERO_PAGE)
        fillOpInfo(opTable(&H94), "STY", 2, AddressingMode.ZERO_PAGE_INDEXED_X)
        fillOpInfo(opTable(&H8C), "STY", 3, AddressingMode.ABSOLUTE)

        'TAX Transfer A to X
        fillOpInfo(opTable(&HAA), "TAX", 1, AddressingMode.IMPLICIT)

        'TAY Transfer A to Y
        fillOpInfo(opTable(&HA8), "TAY", 1, AddressingMode.IMPLICIT)

        'TSX Transfer stack pointer to X
        fillOpInfo(opTable(&HBA), "TSX", 1, AddressingMode.IMPLICIT)

        'TXA Transfer X to A
        fillOpInfo(opTable(&H8A), "TXA", 1, AddressingMode.IMPLICIT)

        'TXS Transfer X to stack pointer
        fillOpInfo(opTable(&H9A), "TXS", 1, AddressingMode.IMPLICIT)

        'TYA Transfer Y to A
        fillOpInfo(opTable(&H98), "TYA", 1, AddressingMode.IMPLICIT)
    End Sub

    Private Sub fillOpInfo(ByRef op As opInfo, name As String, length As Byte, mode As AddressingMode)
        op.name = name
        op.length = length
        op.mode = mode
    End Sub


End Module
