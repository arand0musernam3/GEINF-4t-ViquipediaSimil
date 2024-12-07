package segonaEntrega.mapreduce

import segonaEntrega.tools.{ProcessFiles, Timer}
import segonaEntrega.firstSubmission.DocumentSimilarity
import segonaEntrega.tools.ProcessFiles.{ViquipediaFile, filterViquipediaFile}

import java.io.File

object MappingReduceFunctions {

    // Counts the number of references per document
    def mappingCountReferences(file: File, unusedList: List[Any]): List[(File, Int)] = {
        val refs = ProcessFiles.parseViquipediaFile(file.getPath).refs

        List((file, refs.size))
    }
    def reduceCountReferences(file: File, refs: List[Int]): (File, Int) = {
        (file, refs.sum)
    }

    // Filters words and references in the content of a ViquipediaFile
    def mappingFilterDocuments(vf: ViquipediaFile, unusedList: List[Any]): List[(ViquipediaFile, List[Any])] = {
        List((filterViquipediaFile(vf), unusedList))
    }
    def reduceFilterDocuments(vf: ViquipediaFile, unusedList: List[List[Any]]): (ViquipediaFile, Nil.type ) = {
        (vf, Nil)
    }

    // Filters documents that contain the "query"
    def mappingFilterContains(query: String, file: File, unusedList: List[Any]): List[(ViquipediaFile, Boolean)] = {
        val vf = ProcessFiles.parseViquipediaFile(file.getPath)
        val filteredQuery = DocumentSimilarity.filterWords(query)
        val aux = DocumentSimilarity.filterWords(vf.content)
        List((vf, filteredQuery.forall(aux.contains(_))))
    }
    def reduceFilterContains(file: ViquipediaFile, containsKeyword: List[Boolean]): (ViquipediaFile, Boolean) = {
        (file, containsKeyword.forall(bool => bool))
    }

    // Calculates the PR score of a given document
    def mappingCalculatePR(filePR: (String, Double), refs: List[String]): List[(String, Double)] = {
        filePR match {
            case (doc, pr) =>
                if (refs.isEmpty) List((doc, 0.0))
                else refs.map(ref => (ref, pr / refs.size)) :+ (doc, 0.0)
        }
    }
    def reduceCalculatePR(totalNumberOfPages: Int, brakeFactor: Double, page: String, weights: List[Double]): (String, Double) = {
        val basePR = (1 - brakeFactor) / totalNumberOfPages
        val accumulatedPR = weights.sum * brakeFactor
        (page, basePR + accumulatedPR)
    }

    // Obtains documents that are mutually referenced
    def mappingObtainMutuallyRefDocuments(file: ViquipediaFile, unusedList: List[Any]): List[((String, String), Boolean)] = {
        if (file.refs.isEmpty)
            List(((file.title, ""), false))
        else
            file.refs.flatMap(ref => List(((file.title, ref), true), ((ref, file.title), false)))
    }
    def reduceObtainMutuallyRefDocuments(docs: (String, String), values: List[Boolean]): ((String, String), Boolean) = {
        (docs, values.contains(true) && values.contains(false))
    }

    // Calculates the word frequency of every document
    def mappingCalculateWordFreq(file: ViquipediaFile, unusedList: List[Any]): List[((ViquipediaFile, String), Int)] = {
        file.content.split("\\W").flatMap(word => List(((file, word), 1))).toList
    }
    def reduceCalculateWordFreq(fileWord: (ViquipediaFile, String), counter: List[Int]): ((ViquipediaFile, String), Int) = {
        (fileWord, counter.sum)
    }

    // Calculates the idf per word
    def mappingCalculateInvDocFreq(content: String, unusedList: List[Any]): List[(String, Int)] = {
        (for (word <- content.split("\\W").distinct) yield (word, 1)).toList
    }
    def reduceCalculateInvDocFreq(numberOfDocs: Int, word: String, counter: List[Int]): (String, Double) = {
        val sum = counter.sum
        (word, Math.log10(numberOfDocs.toDouble / sum))
    }

    // Calculates the tf_idf per document
    def mappingTfIdfPerDoc(inverseFreqs: Map[String, Double], wordCountPerDoc: ((String, String), Int), unusedList: List[Any]): List[(String, (String, Double))] = {
        wordCountPerDoc match {
            case ((doc, word), freq) =>
                List((doc, (word, freq * inverseFreqs.getOrElse(word, 0d))))
        }
    }
    def reduceTfIdfPerDoc(doc: String, wordTfIdfList: List[(String, Double)]): (String, Map[String, Double]) = {
        (doc, wordTfIdfList.toMap)
    }

    // Calculates the similarity between all pairs of documents
    def mappingSimilarity(tfIdfPerDoc: Map[String, Map[String, Double]], pair: (String, String), unusedList: List[Any]): List[((String, String), (Map[String, Double], Map[String, Double]))] = {
        //tf_idf vectors
        val doc1Map = tfIdfPerDoc.getOrElse(pair._1, Map.empty)
        val doc2Map = tfIdfPerDoc.getOrElse(pair._2, Map.empty)

        List(((pair._1, pair._2), (doc1Map, doc2Map)))
    }

    def reduceSimilarity(docPair: (String, String), tfidfs: List[(Map[String, Double], Map[String, Double])]): ((String, String), Double) = {
        // Obtenir els maps de la mapping function
        val tfidf1 = tfidfs.head._1
        val tfidf2 = tfidfs.head._2

        // Acumuladors
        var dotProduct = 0.0
        var magnitude1 = 0.0
        var magnitude2 = 0.0

        // Iterar primer tf-idf
        for ((term, value1) <- tfidf1) {
            val value2 = tfidf2.getOrElse(term, 0.0)

            // Actualitzar dotProduct i magnituds.
            dotProduct += value1 * value2
            magnitude1 += value1 * value1
        }

        // Iterar segon tf-idf
        for (value2 <- tfidf2.values) {
            magnitude2 += value2 * value2
        }

        // Calcul final
        val similarity =
            if (magnitude1 == 0 || magnitude2 == 0) 0.0
            else dotProduct / (math.sqrt(magnitude1) * math.sqrt(magnitude2))

        (docPair, similarity)
    }
}
