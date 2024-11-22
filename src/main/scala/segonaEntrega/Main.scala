package segonaEntrega
import firstSubmission.DocumentSimilarity
import segonaEntrega.mapreduce.MappingReduceFunctions
import tools.{MRWrapper, ProcessFiles, Timer}

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
        val result = Timer.timeMeasurement({
            //TODO FIX THIS MESS (pues ni tan mal)
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, Nil),
                MappingReduceFunctions.mappingCountReferences,
                MappingReduceFunctions.reduceCountReferences)
        })

        //print(result)

        val averageReferenceCount = if (result.nonEmpty) result.values.sum.toDouble / result.size else 0.0d
        println(f"\n\nAverage number of unique references: $averageReferenceCount%.2f")
    }

    // Placeholder for reading and running a query from the keyboard
    private def readAndRunQuery(): Unit = {
        println("Please enter your query:")
        val query = readLine()
        println(s"Running your code with query: $query")
        val result = Timer.timeMeasurement({
            //TODO FIX THIS MESS
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, List()),
                MappingReduceFunctions.mappingFilterNGrama(query, _, _),
                MappingReduceFunctions.reduceFilterNGrama)
        })
//
        print(result)

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
                countAverageReferences()

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
    System.exit(0)
}
