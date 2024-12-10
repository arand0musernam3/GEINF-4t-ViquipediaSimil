package segonaEntrega

import pda.practica.DocumentSimilarity
import segonaEntrega.mapreduce.MappingReduceFunctions
import segonaEntrega.tools.ProcessFiles.ViquipediaFile
import tools.{MRWrapper, ProcessFiles, Timer}

import scala.io.StdIn.readLine
import scala.util.Try

object Main extends App {

    private def countAverageReferences(): Unit = {
        println("Counting the average number of references...")
        print("Time spent calculating average number of references (and reading files): ")
        val result = Timer.timeMeasurement({
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, Nil),
                MappingReduceFunctions.mappingCountReferences,
                MappingReduceFunctions.reduceCountReferences)
        })

        val averageReferenceCount = if (result.nonEmpty) result.values.sum.toDouble / result.size else 0.0d
        println(f"\n\nAverage number of unique references: $averageReferenceCount%.2f")
    }

    private def computePageRank(
                                   filteredFiles: List[ViquipediaFile],
                                   steps: Int  = 5,
                                   epsilon: Double = 1e-3,
                               ): List[((String, Double), List[String])] = {
        val totalPages = filteredFiles.size
        var aux = filteredFiles.map(vf => ((vf.title, 1.0 / totalPages), vf.refs))
        var remainingSteps = steps

        while (remainingSteps > 0) {
            val ret = MRWrapper.MR(
                aux,
                MappingReduceFunctions.mappingCalculatePR,
                MappingReduceFunctions.reduceCalculatePR(totalPages, 0.85, _, _)
            )

            val newAux = filteredFiles.map { vf =>
                val newPR = ret.getOrElse(vf.title, 0.0)
                ((vf.title, newPR), vf.refs)
            }

            println(s"Step ${steps - remainingSteps + 1}: ${newAux.map(_._1).sortBy(-_._2).take(4)}")

            if (newAux.forall {
                case ((title, newPR), _) =>
                    val oldPR = aux.find(_._1._1 == title).map(_._1._2).getOrElse(0.0)
                    Math.abs(newPR - oldPR) <= epsilon
            }) {
                remainingSteps = 0
            } else {
                remainingSteps -= 1
            }
            aux = newAux
        }
        aux
    }

    private def recommendationBasedOnQuery(): Unit = {
        println("Please enter your query:")
        val query = readLine().toLowerCase.trim
        println()

        Timer.timeMeasurement({
            val occurrencesPerFile = MRWrapper.MR(
                ProcessFiles.getListOfFiles("viqui_files").map(file => (file, Nil)),
                MappingReduceFunctions.mappingFilterContains(query, _, _),
                MappingReduceFunctions.reduceFilterContains
            )


            val filteredFiles = occurrencesPerFile.filter(_._2).keys.toList

            if (filteredFiles.isEmpty) {
                println("Query was not found in any of the documents.")
            }
            else if (filteredFiles.size == 1) {
                println(s"Only one document matches this query: ${filteredFiles.head.title}")
            }
            else {
                val PRvalue = computePageRank(filteredFiles)
                println("\nHIGHEST PAGE RANK DOCUMENTS (4):")
                PRvalue.map(p => (p._1._1, p._1._2)).sortBy(-_._2).take(4).foreach(p => println(f"${p._1}%-50s: ${p._2}%.3e"))
                println("=======")
                println("\n\nLIST OF NON-MUTUALLY REFERENCED DOCUMENTS WITH > 0.5 COSINE SIMILARITY:")
                Timer.timeMeasurement {
                    similarNonMutuallyReferencedDocuments(PRvalue
                        .sortBy(-_._1._2)
                        .take(nonMutuallyReferenced)
                        .map(_._1._1)
                        .flatMap(title => filteredFiles.find(_.title == title))
                    )
                    print("\nSIMILARITY CALCULATION: ")}
            }
            print("PR + SIMILARITY CALCULATION: ")
        })
        println("=======")
    }

    private def similarNonMutuallyReferencedDocuments(PRs: List[ViquipediaFile]): Unit = {

        val contentFilteredDocuments =
            MRWrapper.MR(
                PRs.map(p => (p, Nil)),
                MappingReduceFunctions.mappingFilterDocuments,
                MappingReduceFunctions.reduceFilterDocuments
            )


        val allDocs = contentFilteredDocuments.keys
        val allDocTitles = allDocs.map(_.title)

        // Calculate mutually referenced document pairs
        val mutuallyReferencedDocPairs =
            MRWrapper.MR(
                    contentFilteredDocuments.toList,
                    MappingReduceFunctions.mappingObtainMutuallyRefDocuments,
                    MappingReduceFunctions.reduceObtainMutuallyRefDocuments
                )
                .filter(_._2).keySet
                .map { case (a, b) => if (a < b) (a, b) else (b, a) }


        // Generate all possible document pairs and filter out mutually referenced pairs
        val nonMutuallyReferencedDocPairs = {
            val allDocPairs = for {a <- allDocTitles; b <- allDocTitles if a < b} yield (a, b)
            allDocPairs.toSet.diff(mutuallyReferencedDocPairs)
        }

        // Extract unique documents from non-mutually referenced pairs
        val nonMutuallyReferencedDocs = nonMutuallyReferencedDocPairs.flatMap {
            case (doc1Title, doc2Title) =>
                Seq(
                    allDocs.find(_.title == doc1Title).get,
                    allDocs.find(_.title == doc2Title).get
                )
        }

        // Calculate word frequency (TF)
        val wordFreq = MRWrapper.MR(
            nonMutuallyReferencedDocs.toList.map(doc => (doc, Nil)),
            MappingReduceFunctions.mappingCalculateWordFreq,
            MappingReduceFunctions.reduceCalculateWordFreq
        )


        // Calculate inverse document frequency (IDF)
        val documentInverseFreq = MRWrapper.MR(
            nonMutuallyReferencedDocs.toList.map(doc => (doc.content, Nil)),
            MappingReduceFunctions.mappingCalculateInvDocFreq,
            MappingReduceFunctions.reduceCalculateInvDocFreq(nonMutuallyReferencedDocs.size, _, _)
        )


        // Calculate TF-IDF
        val tfIdfPerDoc = MRWrapper.MR(
            wordFreq.map { case ((doc, word), freq) =>
                (((doc.title, word), freq), Nil)
            }.toList,
            MappingReduceFunctions.mappingTfIdfPerDoc(documentInverseFreq, _, _),
            MappingReduceFunctions.reduceTfIdfPerDoc
        )


        // Calculate cosine similarity between document pairs
        MRWrapper.MR(
                nonMutuallyReferencedDocPairs.map { case (doc1Title, doc2Title) =>
                    ((doc1Title, doc2Title), Nil)
                }.toList,
                MappingReduceFunctions.mappingSimilarity(tfIdfPerDoc, _, _),
                MappingReduceFunctions.reduceSimilarity
            )
            .filter(_._2 >= 0.5)
            .toList
            .sortBy(-_._2)
            .foreach(p => println(f"${p._1._1.take(60)}%-60s - ${p._1._2.take(60)}%-60s : ${p._2}%.4f"))
        println("=======")
    }

    private def toggleNumberOfMappers(): Unit = {
        println(s"Enter the number of mappers (actual = ${MRWrapper.mapperNumber}): ")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of actors. Please enter a valid integer greater than 0.")

        } else if (newNumActors != MRWrapper.mapperNumber) {
            MRWrapper.mapperNumber = newNumActors
            println(s"Number of mappers set to ${MRWrapper.mapperNumber}.")
        }
    }

    private def toggleNumberOfReducers(): Unit = {
        println(s"Enter the number of reducers (actual = ${MRWrapper.reducerNumber}): ")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of actors. Please enter a valid integer greater than 0.")

        } else if (newNumActors != MRWrapper.reducerNumber) {
            MRWrapper.reducerNumber = newNumActors
            println(s"Number of reducers set to ${MRWrapper.reducerNumber}.")
        }
    }

    private def toggleNumberOfDocuments(): Unit = {
        println(s"Enter the number of non mutually referenced documents (actual = $nonMutuallyReferenced): ")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of documents. Please enter a valid integer greater than 0.")

        } else if (newNumActors != nonMutuallyReferenced) {
            nonMutuallyReferenced = newNumActors
            println(s"Number of documents set to $nonMutuallyReferenced.")
        }
    }

    private var nonMutuallyReferenced: Int = 100
    private var continue = true

    while (continue) {
        println("Select an option:")
        println("1. Count the average number of references of all documents")
        println("2. Recommendation based on query")
        println(s"3. Toggle number of mappers (${MRWrapper.mapperNumber})")
        println(s"4. Toggle number of reducers (${MRWrapper.reducerNumber})")
        println(s"5. Toggle number of non mutually referenced documents to look for ($nonMutuallyReferenced)")
        println("6. Quit")
        print("Option: ")

        val choice = readLine().trim

        choice match {
            case "1" =>
                countAverageReferences()

            case "2" =>
                recommendationBasedOnQuery()

            case "3" =>
                toggleNumberOfMappers()

            case "4" =>
                toggleNumberOfReducers()

            case "5" =>
                toggleNumberOfDocuments()
            case "6" =>
                println("Exiting...")
                continue = false

            case _ =>
                println("Invalid choice. Please try again.")
        }
        println()
    }

    System.exit(0)
}
