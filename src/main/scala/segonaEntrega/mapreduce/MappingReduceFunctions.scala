package segonaEntrega.mapreduce
import segonaEntrega.tools.ProcessFiles

import java.io.File

object MappingReduceFunctions {
    def mappingCountReferences(file: File, unusedList: List[String]): List[(File,String)] = {
        val refs = ProcessFiles.parseViquipediaFile(file.getPath).refs

        refs.map(ref => (file, ref))
    }

    def reduceCountReferences(file: File, refs: List[String]): (File, Int) = {
        (file, ProcessFiles.filterViquipediaReferences(refs).distinct.length)
    }

    //def mappingQueryRecommendation(query: String, file: File, unusedList: List[String]): List[(File, String)] = {
//
    //}

}
