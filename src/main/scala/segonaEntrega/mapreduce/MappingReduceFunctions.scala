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
}
