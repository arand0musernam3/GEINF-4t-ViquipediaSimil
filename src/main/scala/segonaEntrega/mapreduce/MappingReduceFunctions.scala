package segonaEntrega.mapreduce
import segonaEntrega.tools.ProcessFiles
import segonaEntrega.firstSubmission.DocumentSimilarity

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

    def mappingFilterNGrama(query: String, file: File, unusedList: List[Any]): List[(File, (Seq[(String, Int)], String))] = {
        val content = ProcessFiles.parseViquipediaFile(file.getPath).content
        val filteredQuery = DocumentSimilarity.filterWords(query)

        val ngrama = DocumentSimilarity.ngrames(filteredQuery.length, content, print = false)

        List((file, (ngrama, filteredQuery.mkString(" "))))
    }

    def reduceFilterNGrama(file: File, ngrames: List[(Seq[(String, Int)], String)]): (File, Int) = {
        (file, ngrames.head._1.count(_._1 == ngrames.head._2))
    }

}
