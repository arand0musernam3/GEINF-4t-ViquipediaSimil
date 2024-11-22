package segonaEntrega.tools
import scala.util.{Try, Success, Failure}

object Timer {
    def timeMeasurement[A](f: => A): A = {
        // start time
        val startTime = System.nanoTime()

        //execution
        val result = Try(f)

        //end time
        val endTime = System.nanoTime()

        val elapsedTime = (endTime - startTime) / 1_000_000_000.0d // ns to s

        println(f"Execution took $elapsedTime%.4f s")

        result match {
            case Success(value) => value
            case Failure(e) => throw e
        }
    }
}