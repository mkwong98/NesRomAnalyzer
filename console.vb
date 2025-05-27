Module console

    Public Sub init()
        cpu.init()
        rom.init()
    End Sub

    Public Sub powerOn()
        cpu.powerOn()
        memory.powerOn()
    End Sub

    Public Sub setUpForTask(t As taskToRun)
        cpu.setupForTask(t)
        rom.setupMapperConfig(t.mapperConfig)
    End Sub

    Public Sub run()
        cpu.run()
    End Sub


End Module
