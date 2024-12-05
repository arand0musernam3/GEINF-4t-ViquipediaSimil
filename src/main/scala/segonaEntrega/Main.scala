package segonaEntrega

import pda.practica.DocumentSimilarity
import segonaEntrega.mapreduce.MappingReduceFunctions
import segonaEntrega.tools.ProcessFiles.ViquipediaFile
import tools.{MRWrapper, ProcessFiles, Timer}

import scala.io.StdIn.readLine
import scala.util.Try

/*
    TODO:
     - Fer el document per Typst
     - Preguntar tema de les referències a en Mateu (del [[ | ]])
     - Fer script de 1, 4, 10 i 20 actors
     - Ampliar a més de 100 pàgines.
 */

object Main extends App {

    private def shutdownActors(): Unit = {
        MRWrapper.stopSystem()
    }

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

    private def recommendationBasedOnQuery(): Unit = {
        println("Please enter your query:")
        val query = readLine().toLowerCase.trim
        println()
/*
        var proves: List[(ProcessFiles.ViquipediaFile, Boolean)] = List()
        //proves = proves.appended((ViquipediaFile(title = "B", content = "guerra enciam hola", refs = List("C", "A"), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "C", content = "guerra tomata tomata", refs = List("A"), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "D", content = "guerra pa", refs = List("A", "B", "C", "E"), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "A", content = "guerra enciam tomata pa hola", refs = List(), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "E", content = "guerra pastanaga hola", refs = List("D"), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "F", content = "guerra pastanaga hola", refs = List("E"), file = null), true))
        //proves = proves.appended((ViquipediaFile(title = "G", content = "guerra enciam", refs = List("D"), file = null), true))

        proves = proves.appended((ViquipediaFile(title = "B", content = "guerra [[enciam]] hola", refs = List("C", "A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "A", content = "guerra enciam [[hola | aixo es text]]", refs = List("C", "A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "Q", content = "guerra enciam [[#pollon]]", refs = List("C", "A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "Z", content = "guerra enciam [[hola | aixo | es text]] [[polla#gorda]]", refs = List("C", "A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "C", content = "[[Fitxer:Democracy claims.svg|right|350px|thumb|Des de la [[Segona Guerra Mundial]], la democràcia ha guanyat una àmplia acceptació. Aquest mapa mostra l'auto-identificació oficial feta per governs de tot el món en relació a la democràcia, el 2012. Mostra l'estatus legal de la democràcia en el món. {{legend|green|Governs que s'identifiquen com a democràtics}} {{legend|red|Governs que no s'identifiquen com a democràtics: [[Ciutat del Vaticà]], [[Oman]], [[Aràbia Saudita]], [[Emirats Àrabs Units]], [[Brunei]] i [[Fiji]].}}]]", refs = List("C", "A"), file = null), true))

        val occurrencesPerFile = Timer.timeMeasurement({
            //TODO FIX THIS MESS
            MRWrapper.MR(proves.map{ case (file, _) => (file, Nil)},
                MappingReduceFunctions.testMapping(query,_,_),
                MappingReduceFunctions.reduceFilterContains)
        })

        val filteredProves = occurrencesPerFile.filter(_._2).toList.map(_._1)

        // TODO: modificar PR perquè retorni directament ViquipediaFile, no només l'string del títol
        val PRvalue = Timer.timeMeasurement({
            var aux = filteredProves
                .map(vf => ((vf.title, 1.0d / proves.size), vf.refs))
            val epsilon = 1e-3
            var steps = 5
            while (steps > 0) {
                val ret = MRWrapper.MR(aux,
                    MappingReduceFunctions.mappingCalculatePR,
                    MappingReduceFunctions.reduceCalculatePR(proves.length, 0.85, _, _)
                )
                val newAux = filteredProves.map(vf => ((vf.title, ret.getOrElse(vf.title, 0d)), vf.refs))
                println(s"Step $steps: $newAux")
                if (newAux.forall { case ((str, pr), _) => epsilon > Math.abs(pr - aux.find { case ((str2, _), _) => str == str2 }.get._1._2) }) {
                    steps = 0
                } else {
                    steps = steps - 1
                }
                aux = newAux
            }
            aux
        })
        println(PRvalue.map(_._1))
        //TODO quan arreglat PR, també arreglar això
        similarNonMutuallyReferencedDocuments(PRvalue.sortBy(-_._1._2).take(100).map {case ((name, pr), refs) => ((proves.find(_._1.title == name).get._1, pr), refs)})
*/

        val occurrencesPerFile = Timer.timeMeasurement({
            //TODO FIX THIS MESS
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, List()),
                MappingReduceFunctions.mappingFilterContains(query,_,_),
                MappingReduceFunctions.reduceFilterContains)
        })


        val filteredFiles = occurrencesPerFile.filter(_._2).toList.map(_._1)

        println(filteredFiles)

        if (filteredFiles.isEmpty) {
            println("Query was not found in any of the documents.")
        }
        else if (filteredFiles.size == 1) {
            println(s"Only one document matches this query: ${filteredFiles.head.title}")
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
            println(PRvalue.map(_._1).sortBy(-_._2))

            similarNonMutuallyReferencedDocuments(PRvalue.sortBy(-_._1._2).take(100).map {case ((name, pr), refs) => ((occurrencesPerFile.find(_._1.title == name).get._1, pr), refs)})
        }
    }

    private def similarNonMutuallyReferencedDocuments(PRs: List[((ViquipediaFile, Double), List[String])]): Unit = {

        val input = PRs.map { case ((doc, _), refs) => (doc, refs) }
        val allDocs = PRs.map(_._1._1)
        val allDocTitles = allDocs.map(_.title)

        // Calculate mutually referenced document pairs
        val mutuallyReferencedDocPairs = Timer.timeMeasurement {
            val areDocsReferenced = MRWrapper.MR(
                input,
                MappingReduceFunctions.mappingObtainNonMutuallyRefDocuments,
                MappingReduceFunctions.reduceObtainNonMutuallyRefDocuments
            )
            areDocsReferenced
              .filter(_._2) // Keep only mutually referenced pairs
              .map { case ((a, b), _) => if (a < b) (a, b) else (b, a) }
              .toSet
        }

        // Generate all possible document pairs and filter out mutually referenced pairs
        val nonMutuallyReferencedDocPairs = {
            val allDocPairs = for { a <- allDocTitles; b <- allDocTitles if a < b } yield (a, b)
            allDocPairs.toSet.diff(mutuallyReferencedDocPairs).toList
        }

        // Extract unique documents from non-mutually referenced pairs
        val nonMutuallyReferencedDocs = Set(nonMutuallyReferencedDocPairs.flatMap {
            case (doc1Title, doc2Title) =>
                Seq(
                    allDocs.find(_.title == doc1Title).get,
                    allDocs.find(_.title == doc2Title).get
                )
        }: _*)

        // Calculate word frequency (TF)
        val wordFreq = Timer.timeMeasurement {
            MRWrapper.MR(
                nonMutuallyReferencedDocs.toList.map(doc => (doc, Nil)),
                MappingReduceFunctions.mappingCalculateWordFreq,
                MappingReduceFunctions.reduceCalculateWordFreq
            )
        }


        // Calculate inverse document frequency (IDF)
        val documentInverseFreq = Timer.timeMeasurement {
            MRWrapper.MR(
                nonMutuallyReferencedDocs.toList.map(doc => (doc.content, Nil)),
                MappingReduceFunctions.mappingCalculateInvDocFreq,
                MappingReduceFunctions.reduceCalculateInvDocFreq(nonMutuallyReferencedDocs.size, _, _)
            )
        }


        // Calculate TF-IDF
        val tfIdfPerWord = Timer.timeMeasurement {
            MRWrapper.MR(
                wordFreq.map { case ((doc, word), freq) =>
                    (((doc, word), freq), Nil)
                }.toList,
                MappingReduceFunctions.mappingTfIdfPerDoc(documentInverseFreq,_,_),
                MappingReduceFunctions.reduceTfIdfPerDoc
            )
        }


        // Calculate cosine similarity between document pairs
        val similarityPairs = Timer.timeMeasurement {
            MRWrapper.MR(
                nonMutuallyReferencedDocPairs.map { case (doc1Title, doc2Title) =>
                    val doc1 = nonMutuallyReferencedDocs.find(_.title == doc1Title).get
                    val doc2 = nonMutuallyReferencedDocs.find(_.title == doc2Title).get
                    ((doc1, doc2), Nil)
                },
                MappingReduceFunctions.mappingSimilarity(tfIdfPerWord,_,_),
                MappingReduceFunctions.reduceSimilarity
            )
        }

        similarityPairs
          .filter(_._2 >= 0.5)
          .toList
          .sortBy(-_._2)
          .foreach(println)
    }

    private def toggleNumberOfMappers(): Unit = {
        println(s"Enter the number of mappers (actual = $numMappers): ")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of actors. Please enter a valid integer greater than 0.")

        } else if (newNumActors != numMappers) {
            numMappers = newNumActors
            println(s"Number of mappers set to $numMappers.")
            //TODO enviar canvi de nombre d'actors i que s'espavili la classe que ho gestiona, pensar bé com fer-ho.
        }
    }

    private def toggleNumberOfReducers(): Unit = {
        println(s"Enter the number of reducers (actual = $numReducers): ")
        val input = readLine()
        val newNumActors = Try(input.toInt).getOrElse(-1)

        if (newNumActors <= 0) {
            println("Invalid number of actors. Please enter a valid integer greater than 0.")

        } else if (newNumActors != numReducers) {
            numReducers = newNumActors
            println(s"Number of reducers set to $numReducers.")
            //TODO enviar canvi de nombre d'actors i que s'espavili la classe que ho gestiona, pensar bé com fer-ho.
        }
    }

    private var numMappers: Int = 1
    private var numReducers: Int = 1
    private var continue = true

    while (continue) {
        println("Select an option:")
        println("1. Count the average number of references of all documents")
        println("2. Recommendation based on query")
        println(s"3. Toggle number of mappers ($numMappers)")
        println(s"4. Toggle number of reducers ($numReducers)")
        println("5. Quit")
        print("Option: ")

        val choice = readLine()

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
