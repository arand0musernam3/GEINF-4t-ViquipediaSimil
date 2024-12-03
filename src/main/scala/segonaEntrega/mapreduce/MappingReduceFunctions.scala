package segonaEntrega.mapreduce

import segonaEntrega.tools.{PageRank, ProcessFiles}
import segonaEntrega.firstSubmission.DocumentSimilarity
import segonaEntrega.tools.ProcessFiles.ViquipediaFile

import java.io.File

object MappingReduceFunctions {
    def mappingCountReferences(file: File, unusedList: List[Any]): List[(File, Int)] = {
        val refs = ProcessFiles.parseViquipediaFile(file.getPath).refs

        List((file, refs.size))
    }

    def reduceCountReferences(file: File, refs: List[Int]): (File, Int) = {
        (file, refs.sum)
        //refs.head because we know that there is only going to be one list.
    }


    def mappingFilterNGrama(query: String, file: File, unusedList: List[Any]): List[(ViquipediaFile, (Seq[(String, Int)], String))] = {
        val vf = ProcessFiles.parseViquipediaFile(file.getPath)
        val filteredQuery = DocumentSimilarity.filterWords(query)
        val ngrama = DocumentSimilarity.ngrames(filteredQuery.length, vf.content, print = false, sort = false)

        List((vf, (ngrama, filteredQuery.mkString(" "))))
    }

    def reduceFilterNGrama(file: ViquipediaFile, ngrames: List[(Seq[(String, Int)], String)]): (ViquipediaFile, Int) = {
        (file, ngrames.head._1.find(_._1 == ngrames.head._2) match {
            case Some((_, value)) => value
            case None => 0
        })
    }

    def mappingFilterContains(query: String, file: File, unusedList: List[Any]): List[(ViquipediaFile, Boolean)] = {
        val vf = ProcessFiles.parseViquipediaFile(file.getPath)
        val filteredQuery = DocumentSimilarity.filterWords(query)

        List((vf, DocumentSimilarity.filterWords(vf.content).contains(query)))
    }

    def reduceFilterContains(file: ViquipediaFile, containsKeyword: List[Boolean]): (ViquipediaFile, Boolean) = {
        (file, containsKeyword.forall(bool => bool))
    }

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

    def mappingObtainNonMutuallyRefDocuments(file: ViquipediaFile, refs: List[String]): List[((String, String), Boolean)] = {
        refs.flatMap(ref => List(((file.title, ref), true), ((ref, file.title), false)))
    }

    def reduceObtainNonMutuallyRefDocuments(docs: (String, String), values: List[Boolean]): ((String, String), Boolean) = {
        (docs, values.contains(true) && values.contains(false))
    }

    def mappingCalculateTF_IDF(docs: (String, String), unusedList: List[Any]): Unit = {

    }
    //def mappingTwoPagesSimilarNonReferenced(file: ViquipediaFile, comparison: List[ViquipediaFile]) : ((ViquipediaFile, ViquipediaFile), Double)

    def mappingCalculateWordFreq(file: ViquipediaFile, unusedList: List[Any]): List[((String, String), Int)] = {
        DocumentSimilarity.filterWords(file.content).flatMap(word => List(((file.title, word), 1))).toList
    }

    def reduceCalculateWordFreq(fileWord: (String, String), counter: List[Int]): ((String, String), Int) = {
        (fileWord, counter.sum)
    }

    def mappingCalculateInvDocFreq(content: String, unusedList: List[Any]): List[(String, Int)] = {
        (for (word <- content.split(" ")) yield (word, 1)).distinct.toList
    }

    def reduceCalculateInvDocFreq(numberOfDocs: Int, word: String, counter: List[Int]): (String, Double) = {
        val sum = counter.sum
        (word, Math.log(numberOfDocs.toDouble / sum))
    }

    def mappingTfIdfPerDoc(wordCountPerDoc: ((String, String), Int), inverseFreqs: List[(String, Double)]) = {
        wordCountPerDoc match {
            case ((doc, word), freq) =>
                List(((doc, word), freq * inverseFreqs.toMap.getOrElse(word, 0d)))
        }
    }

    def reduceTfIdfPerDoc(docWord: (String, String), tfIdf: List[Double]) = {
        //println(s"$docWord - $tfIdf")
        (docWord, tfIdf.head)
    }

    def mappingSimilarity(pair: (ViquipediaFile, ViquipediaFile), tfIdfPerWord: List[((String, String), Double)])
    : List[((String, String), (List[(String, Double)], List[(String, Double)]))] = {
        /*List((
            (pair._1.title, pair._2.title), //docPair
            (
                tfIdfPerWord.filter { case ((doc, _), _) => doc == pair._1.title }
                    .map { case ((_, word), tfidf) => (word, tfidf) },
                tfIdfPerWord.filter { case ((doc, _), _) => doc == pair._2.title }
                    .map { case ((_, word), tfidf) => (word, tfidf) }
            ) //listsOfTfIdsPerElement
        ))*/

        val doc1TfIdf = tfIdfPerWord.collect {
            case ((doc, word), tfidf) if doc == pair._1.title => (word, tfidf)
        }

        val doc2TfIdf = tfIdfPerWord.collect {
            case ((doc, word), tfidf) if doc == pair._2.title => (word, tfidf)
        }

        List(((pair._1.title, pair._2.title), (doc1TfIdf, doc2TfIdf)))
    }

    def reduceSimilarity(docPair: (String, String), tfidfs: List[(List[(String, Double)], List[(String, Double)])]) = {
        val doc1 = docPair._1
        val doc2 = docPair._2
        val tfidf1 = tfidfs.head._1.toMap
        val tfidf2 = tfidfs.head._2.toMap

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
