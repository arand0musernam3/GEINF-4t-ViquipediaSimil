package segonaEntrega.mapreduce
import segonaEntrega.tools.ProcessFiles

object MappingReduceFunctions {
    def mappingCountReferences(files: List[String]): List[(String,String)] = {
        var result : List[(String, String)] = List()
        for (file <- files) {
            result = result.appended((file, ProcessFiles.parseViquipediaFile(file).refs))
        }
        result
    }

    def reduceCountReferences(file: String, refs: List[String]): (String, Int) = {
        (file, refs.distinct.length)
    }

}
