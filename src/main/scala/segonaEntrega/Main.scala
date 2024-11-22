package segonaEntrega
import timer.Timer

object Main extends App{
    //TODO MAIN CODE
    Timer.timeMeasurement({
        println("Inici")
        Thread.sleep(1000)
        println("final")
    })
}
