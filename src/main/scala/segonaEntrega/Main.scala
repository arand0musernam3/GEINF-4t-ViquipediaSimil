package segonaEntrega
import firstSubmission.DocumentSimilarity
import tools.Timer

import scala.io.StdIn.readLine
import scala.util.Try

object Main extends App {

    private def initializeActors(): Unit = {

    }

    private def shutdownActors(): Unit = {

    }

    // Placeholder for your test function
    private def testFunction(): Unit = {
        println("Running test function...")
        Thread.sleep(1000);
    }

    // Placeholder for counting average references
    private def countAverageReferences(): Unit = {
        println("Counting the average number of references...")
        // You will enter the code for this
    }

    // Placeholder for reading and running a query from the keyboard
    private def readAndRunQuery(): Unit = {
        println("Please enter your query:")
        val query = readLine()
        println(s"Running your code with query: $query")

        //TODO
        //Timer.timeMeasurement()
    }

    // Toggle number of actors
    private def toggleNumberOfActors(): Unit = {
        println("Enter the number of actors:")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of actors. Please enter a valid integer greater than 0.")

        } else if (newNumActors != numActors) {
            numActors = newNumActors
            println(s"Number of actors set to $numActors.")
            //TODO enviar canvi de nombre d'actors i que s'espavili la classe que ho gestiona, pensar bé com fer-ho.
        }
        else {
            println(s"Number of actors was already $numActors.")
        }
    }

    private var numActors: Int = 1
    private var continue = true

    initializeActors()

    while (continue) {
        println("Select an option:")
        println("1. Test a function")
        println("2. Count the average number of references")
        println("3. Read a query and run your code")
        println("4. Toggle number of actors")
        println("5. Quit")
        print("Option: ")

        val choice = readLine()

        choice match {
            case "1" =>
                Timer.timeMeasurement(testFunction())

            case "2" =>
                Timer.timeMeasurement(countAverageReferences())

            case "3" =>
                readAndRunQuery()

            case "4" =>
                toggleNumberOfActors()

            case "5" =>
                println("Exiting...")
                continue = false

            case _ =>
                println("Invalid choice. Please try again.")
        }
        println()
    }

    //TODO WRITE ACTOR CLOSING SEQUENCE
    shutdownActors()
}
