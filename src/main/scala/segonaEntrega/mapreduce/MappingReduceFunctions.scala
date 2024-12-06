package segonaEntrega.mapreduce

import segonaEntrega.tools.ProcessFiles
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
    def mappingFilterDocuments(pair: (ViquipediaFile, Double), refs: List[String]): List[(ViquipediaFile, List[String])] = {
        List((filterViquipediaFile(pair._1), refs))
    }
    def reduceFilterDocuments(vf: ViquipediaFile, refs: List[List[String]]): (ViquipediaFile, List[String]) = {
        (vf, refs.head)
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
            case (str, pr) =>
                val newPr = pr / refs.size
                refs.map(ref => (ref, newPr)).appended(str, 0)
        }
    }
    def reduceCalculatePR(totalNumberOfPages: Int, brakeFactor: Double, page: String, weights: List[Double]): (String, Double) = {
        (page, ((1 - brakeFactor) / totalNumberOfPages) + brakeFactor * weights.sum)
    }

    // Obtains documents that are mutually referenced
    def mappingObtainMutuallyRefDocuments(file: ViquipediaFile, refs: List[String]): List[((String, String), Boolean)] = {
        if (refs.isEmpty)
            List(((file.title, ""), false))
        else
            refs.flatMap(ref => List(((file.title, ref), true), ((ref, file.title), false)))
    }
    def reduceObtainMutuallyRefDocuments(docs: (String, String), values: List[Boolean]): ((String, String), Boolean) = {
        (docs, values.contains(true) && values.contains(false))
    }

    // Calculates the word frequency of every document
    def mappingCalculateWordFreq(file: ViquipediaFile, unusedList: List[Any]): List[((String, String), Int)] = {
        DocumentSimilarity.filterWords(file.content).flatMap(word => List(((file.title, word), 1))).toList
    }
    def reduceCalculateWordFreq(fileWord: (String, String), counter: List[Int]): ((String, String), Int) = {
        (fileWord, counter.sum)
    }

    // Calculates the idf per word
    def mappingCalculateInvDocFreq(content: String, unusedList: List[Any]): List[(String, Int)] = {
        (for (word <- DocumentSimilarity.filterWords(content)) yield (word, 1)).distinct.toList
    }
    def reduceCalculateInvDocFreq(numberOfDocs: Int, word: String, counter: List[Int]): (String, Double) = {
        val sum = counter.sum
        (word, Math.log10(numberOfDocs.toDouble / sum))
    }

    // Calculates the tf_idf per document
    def mappingTfIdfPerDoc(inverseFreqs: Map[String, Double], wordCountPerDoc: ((String, String), Int), unusedList: List[Any]): List[((String, String), Double)] = {
        wordCountPerDoc match {
            case ((doc, word), freq) =>
                List(((doc, word), freq * inverseFreqs.getOrElse(word, 0d)))
        }
    }
    def reduceTfIdfPerDoc(docWord: (String, String), tfIdf: List[Double]): ((String, String), Double) = {
        (docWord, tfIdf.head)
    }

    // Calculates the similarity between all pairs of documents
    def mappingSimilarity(tfIdfPerWord: Map[(String, String), Double], pair: (ViquipediaFile, ViquipediaFile), unusedList: List[Any]): List[((String, String), (Map[String, Double], Map[String, Double]))] = {

        val doc1TfIdf = tfIdfPerWord.collect {
            case ((doc, word), tfidf) if doc == pair._1.title => (word, tfidf)
        }

        val doc2TfIdf = tfIdfPerWord.collect {
            case ((doc, word), tfidf) if doc == pair._2.title => (word, tfidf)
        }

        List(((pair._1.title, pair._2.title), (doc1TfIdf, doc2TfIdf)))
    }
    def reduceSimilarity(docPair: (String, String), tfidfs: List[(Map[String, Double], Map[String, Double])]): ((String, String), Double) = {
        val doc1 = docPair._1
        val doc2 = docPair._2
        val tfidf1 = tfidfs.head._1
        val tfidf2 = tfidfs.head._2

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
        val similarity = if (magnitude1 == 0 || magnitude2 == 0)
            0.0
        else
            dotProduct / (magnitude1 * magnitude2)

        ((doc1, doc2), similarity)
    }
}
