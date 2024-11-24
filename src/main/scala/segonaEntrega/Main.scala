package segonaEntrega

import segonaEntrega.mapreduce.MappingReduceFunctions
import segonaEntrega.tools.ProcessFiles.ViquipediaFile
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
        val text = "Alguns exemples: [[Bombardeig de Guernica|Guernica]], [[Albert Einstein]], [[Fitxer:Imatge.jpg]], [[fitxer:Imatge.jpg]] [[#Secció]], [[Pàgina de prova]]"
        val regex = """(?i)\[\[(?!Fitxer:)([^\|\#\]\[]+)(?=\|?|#|\]\])"""

        val matches = regex.r.findAllIn(text).matchData.map(m => m.group(1)).toList

        println(matches) // Output: List(Bombardeig de Guernica, Albert Einstein, Pàgina de prova)
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
    private def recommendationBasedOnQuery(): Unit = {
        println("Please enter your query:")
        val query = readLine().toLowerCase
        println()

        //        var proves: List[(ProcessFiles.ViquipediaFile, Int)] = List()
        //        proves = proves.appended((ViquipediaFile(title = "B", content = "", refs = List("C", "A"), file = null), 1))
        //        proves = proves.appended((ViquipediaFile(title = "C", content = "", refs = List("A"), file = null), 1))
        //        proves = proves.appended((ViquipediaFile(title = "D", content = "", refs = List("A", "B", "C", "E"), file = null), 1))
        //        proves = proves.appended((ViquipediaFile(title = "A", content = "", refs = List(), file = null), 1))
        //
        //        val filteredProves = proves.filter(_._2 > 0).map(_._1)
        //
        //
        //        val PRvalue = Timer.timeMeasurement({
        //            var aux = filteredProves
        //                .map(vf => ((vf.title, 1.0d / proves.size), vf.refs))
        //
        //            val epsilon = 1e-3
        //            var steps = 5
        //            while (steps > 0) {
        //                val ret = MRWrapper.MR(aux,
        //                    MappingReduceFunctions.mappingCalculatePR,
        //                    MappingReduceFunctions.reduceCalculatePR(proves.length, 0.85, _, _)
        //                )
        //
        //                val newAux = filteredProves.map(vf => ((vf.title, ret.getOrElse(vf.title, 0d)), vf.refs))
        //
        //                println(s"Step $steps: $newAux")
        //                if (newAux.forall { case ((str, pr), _) => epsilon > Math.abs(pr - aux.find { case ((str2, _), _) => str == str2 }.get._1._2) }) {
        //                    steps = 0
        //                } else {
        //                    steps = steps - 1
        //                }
        //                aux = newAux
        //            }
        //
        //            aux
        //        })
        //        println(PRvalue.map(_._1))


        val occurrencesPerFile = Timer.timeMeasurement({
            //TODO FIX THIS MESS
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, List()),
                MappingReduceFunctions.mappingFilterNGrama(query, _, _),
                MappingReduceFunctions.reduceFilterNGrama)
        })


        val filteredFiles = occurrencesPerFile.filter(_._2 > 0).toList.map(_._1)

        //filteredFiles.foreach(println(_))

        if (filteredFiles.isEmpty) {
            println("Query was not found in any of the documents.")
        }
        else if (filteredFiles.size == 1) {
            println(s"Only one document matches this query: ${occurrencesPerFile.head._1.title}")
        }
        else {
            val PRvalue = Timer.timeMeasurement({
                var aux = filteredFiles
                    .map(vf => ((vf.title, 1.0d / filteredFiles.size), vf.refs))

                val epsilon = 1e-3
                var steps = 5
                while (steps > 0) {
                    val ret = MRWrapper.MR(aux,
                        MappingReduceFunctions.mappingCalculatePR,
                        MappingReduceFunctions.reduceCalculatePR(filteredFiles.length, 0.85, _, _)
                    )

                    val newAux = filteredFiles.map(vf => ((vf.title, ret.getOrElse(vf.title, 0d)), vf.refs))

                    println(s"Step $steps: ${newAux.map(_._1).sortBy(-_._2).take(4)}")
                    if (newAux.forall { case ((str, pr), _) => epsilon > Math.abs(pr - aux.find { case ((str2, _), _) => str == str2 }.get._1._2) }) {
                        steps = 0
                    } else {
                        steps = steps - 1
                    }
                    aux = newAux
                }

                aux
            })
            println(PRvalue.map(_._1).sortBy(-_._2).take(4))
        }
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
        println("1. Count the average number of references of all documents")
        println("2. Recommendation based on query")
        println("3. Similar documents non-mutually referenced based on query")
        println("4. Toggle number of actors")
        println("5. Quit")
        print("Option: ")

        val choice = readLine()

        choice match {
            case "1" =>
                countAverageReferences()

            case "2" =>
                recommendationBasedOnQuery()

            case "3" =>
                println("TODO")

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
