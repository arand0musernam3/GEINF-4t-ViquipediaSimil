package segonaEntrega.mapreduce
import segonaEntrega.tools.{PageRank, ProcessFiles}
import segonaEntrega.firstSubmission.DocumentSimilarity
import segonaEntrega.tools.ProcessFiles.ViquipediaFile

import java.io.File

object MappingReduceFunctions {
    def mappingCountReferences(file: File, unusedList: List[Any]): List[(File,List[String])] = {
        val refs = ProcessFiles.parseViquipediaFile(file.getPath).refs

        List((file, refs))
    }

    def reduceCountReferences(file: File, refs: List[List[String]]): (File, Int) = {
        (file, ProcessFiles.filterViquipediaReferences(refs.head).distinct.length)
        //refs.head because we know that there is only going to be one list.
    }

    //def mappingCountReferences(file: File, unusedList: List[Any]): List[(File,String)] = {
    //    val refs = ProcessFiles.parseViquipediaFile(file.getPath).refs
    //
    //    refs.map(ref => (file, ref))
    //}
    //
    //def reduceCountReferences(file: File, refs: List[String]): (File, Int) = {
    //    (file, ProcessFiles.filterViquipediaReferences(refs).distinct.length)
    //}

    def mappingFilterNGrama(query: String, file: File, unusedList: List[Any]): List[(ViquipediaFile, (Seq[(String, Int)], String))] = {
        val vf = ProcessFiles.parseViquipediaFile(file.getPath)
        val filteredQuery = DocumentSimilarity.filterWords(query)
        val ngrama = DocumentSimilarity.ngrames(filteredQuery.length, vf.content, print = false, sort = false)

        List((vf, (ngrama, filteredQuery.mkString(" "))))
    }

    def reduceFilterNGrama(file: ViquipediaFile, ngrames: List[(Seq[(String, Int)], String)]): (ViquipediaFile, Int) = {
        (file, ngrames.head._1.find(_._1 == ngrames.head._2) match {
            case Some((_, value)) => value
            case None => 0 //this should never happen but it's here for completeness
        })
    }

    def mappingCalculatePR(file: ViquipediaFile, numberOfOccurrences: Int): (ViquipediaFile, Double) = {
        val filteredRefsFile = ProcessFiles.filterViquipediaReferences(file)
        (filteredRefsFile, PageRank.calculatePageRank(filteredRefsFile))
    }

}
