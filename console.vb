Module console

    Public Sub init()
        cpu.init()
        rom.init()
    End Sub

    Public Sub powerOn()
        cpu.powerOn()
    End Sub

    Public Sub setUpForTask(t As taskToRun)
        cpu.setupForTask(t)
    End Sub

    Public Sub run()
        cpu.run()
    End Sub


End Module
