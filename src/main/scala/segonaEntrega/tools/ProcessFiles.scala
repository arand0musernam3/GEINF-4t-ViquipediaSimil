package segonaEntrega.tools

import mapreduce.ViquipediaParse.ResultViquipediaParsing
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.File
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.xml.{Elem, XML}

object ProcessFiles {

    case class ViquipediaFile(titol: String, contingut: String, refs: List[String])

    def readFile(file: String): String = {
        val initialFile = new File(file)
        val targetStream = FileUtils.openInputStream(initialFile)
        val textString = IOUtils.toString(targetStream, StandardCharsets.UTF_8)
        targetStream.close()
        textString
    }

    def getListOfFiles(dir: String):List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
            d.listFiles.filter(_.isFile).toList
        } else {
            List[File]()
        }
    }

    def showTxtFilesFromDirectory(directori:String): Unit =
    {
        val txtFiles= for (f<-getListOfFiles(directori); name = f.getName if name.takeRight(3)=="txt")
            yield name
        for(f<-txtFiles)
        {
            println(f)
            println(readFile(directori++"/"++f))
            println("--------------------------------------------------")
        }
    }

    def parseViquipediaFile(filename: String): ViquipediaFile = {
        val xmlleg = new java.io.InputStreamReader(new java.io.FileInputStream(filename), "UTF-8")

        // Agafo el document XML i ja està internament estructurat per anar accedint als camps que volguem
        val xmllegg: Elem = XML.load(xmlleg)

        val title = (xmllegg \\ "title").text
        val content = (xmllegg \\ "text").text

        val refRegex = new Regex("\\[\\[[^\\]]*\\]\\]")
        //println("La pagina es: " + titol)
        //println("i el contingut: ")
        //println(contingut)
        val refs = (refRegex findAllIn content).toList //tot el que està entre [[ ]]

        xmlleg.close()
        ViquipediaFile(title, content, refs)
    }

    def filterViquipediaReferences(file: ViquipediaFile): ViquipediaFile = {
        ViquipediaFile(file.titol, file.contingut, filterViquipediaReferences(file.refs))
    }

    def filterViquipediaReferences(refs: List[String]): List[String] = {
        refs.filterNot(_.contains(':'))
    }
}
