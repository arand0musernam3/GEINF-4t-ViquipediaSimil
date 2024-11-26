package segonaEntrega

import segonaEntrega.firstSubmission.DocumentSimilarity.{ngrames, nonstopfreq}
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

        var proves: List[(ProcessFiles.ViquipediaFile, Boolean)] = List()
        proves = proves.appended((ViquipediaFile(title = "B", content = "guerra enciam", refs = List("C", "A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "C", content = "guerra tomata", refs = List("A"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "D", content = "guerra pa", refs = List("A", "B", "C", "E"), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "A", content = "guerra enciam tomata pa", refs = List(), file = null), true))
        proves = proves.appended((ViquipediaFile(title = "E", content = "guerra pastanaga", refs = List("D"), file = null), true))
        val filteredProves = proves.filter(_._2).map(_._1)
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
        similarNonMutuallyReferencedDocuments(PRvalue.sortBy(-_._1._2).take(100).map {case ((name, pr), refs) => ((proves.find(_._1.title == name).get._1, pr), refs)})

        /*
        val occurrencesPerFile = Timer.timeMeasurement({
            //TODO FIX THIS MESS
            MRWrapper.MR(for (file <- ProcessFiles.getListOfFiles("viqui_files")) yield (file, List()),
                MappingReduceFunctions.mappingFilterContains(query,_,_),
                MappingReduceFunctions.reduceFilterContains)
        })


        val filteredFiles = occurrencesPerFile.filter(_._2).toList.map(_._1)

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

            //similarNonMutuallyReferencedDocuments(PRvalue.sortBy(-_._1._2).take(100))


        }*/
    }

    private def similarNonMutuallyReferencedDocuments(PRs: List[((ViquipediaFile, Double), List[String])]): Unit = {
        import scala.collection.mutable //no sé si podem utilitzar això
        val input = PRs.map { case ((doc, _), refs) => (doc, refs) }
        val allDocs = PRs.map(_._1._1.title)
        val areDocsReferenced = Timer.timeMeasurement({
            MRWrapper.MR(input,
                MappingReduceFunctions.mappingObtainNonMutuallyRefDocuments,
                MappingReduceFunctions.reduceObtainNonMutuallyRefDocuments
            )
        })

        //òptimament això de allDocPairs es podria fer amb un actor mentre els altres actors fan lu altre, podríem mirar
        //d'aprofitar els futures d'alguna forma, això ja optimitzacions per a versió final
        val allDocPairs = for {a <- allDocs; b <- allDocs if a < b} yield (a, b)

        val mutuallyReferencedDocPairs = areDocsReferenced.filter(_._2).map { case ((a, b), _) => if (a < b) (a, b) else (b, a) }.toSet
        println(mutuallyReferencedDocPairs)
        val nonMutuallyReferencedDocPairs = allDocPairs.toSet.diff(mutuallyReferencedDocPairs).toList
        println(nonMutuallyReferencedDocPairs)

        val nonMutuallyReferencedDocs: mutable.Set[String] = mutable.Set()
        nonMutuallyReferencedDocPairs.foreach(pair => nonMutuallyReferencedDocs.addAll(Seq(pair._1, pair._2)))

        val numberOfDocuments = nonMutuallyReferencedDocs.size

        nonMutuallyReferencedDocPairs.foreach{ case (p1,p2) => println(s"${(p1,p2)}\t:${cosineSimTFIDF(p1, p2, )}")}

        /*
        Plantejament de tf_idf (TODO):
            - per a cada document, obtenir totes les paraules que el formen (filtrant links i polles)
            -
         */


        def cosineSimTFIDF(
                              text1: String,
                              text2: String,
                              corpus: List[String],
                              n: Int,
                              stopWords: List[String] = List()
                          ): Double = {

            // Funció per calcular TF
            def tf(text: String, stopWords: List[String]): Map[String, Int] = {
                if (n == 1) nonstopfreq(text, stopWords, print = false).toMap
                else ngrames(n, text, print = false).toMap
            }

            // Funció per calcular DF
            def calculateDF(corpus: List[String], stopWords: List[String]): Map[String, Int] = {
                corpus.flatMap(doc => tf(doc, stopWords).keySet)
                    .groupBy(identity)
                    .view.mapValues(_.size)
                    .toMap
            }

            // Funció per calcular IDF
            def calculateIDF(df: Map[String, Int], corpusSize: Int): Map[String, Double] = {
                df.map { case (term, count) => term -> math.log(corpusSize.toDouble / count) }
            }

            // TF per text1 i text2
            val tf1 = tf(text1, stopWords)
            val tf2 = tf(text2, stopWords)

            // Càlcul de DF a tot el corpus
            val df = calculateDF(corpus, stopWords)

            // IDF per tot el corpus
            val idf = calculateIDF(df, corpus.size)

            // Càlcul de TF-IDF per cada text
            def tfidf(tf: Map[String, Int], idf: Map[String, Double]): Map[String, Double] = {
                tf.map { case (term, freq) => term -> (freq * idf.getOrElse(term, 0.0)) }
            }

            val tfidf1 = tfidf(tf1, idf)
            val tfidf2 = tfidf(tf2, idf)

            // Generació del conjunt unificat de termes
            val allTerms = (tfidf1.keySet ++ tfidf2.keySet).toList

            // Creació de vectors TF-IDF alineats
            val vector1 = allTerms.map(term => tfidf1.getOrElse(term, 0.0))
            val vector2 = allTerms.map(term => tfidf2.getOrElse(term, 0.0))

            // Càlcul del producte escalar i magnituds
            val dotProduct = vector1.zip(vector2).map { case (a, b) => a * b }.sum
            val magnitude1 = math.sqrt(vector1.map(a => a * a).sum)
            val magnitude2 = math.sqrt(vector2.map(b => b * b).sum)

            // Retornar la similitud del cosinus
            if (magnitude1 == 0 || magnitude2 == 0) 0.0
            else dotProduct / (magnitude1 * magnitude2)
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
