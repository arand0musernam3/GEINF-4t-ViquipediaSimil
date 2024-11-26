package segonaEntrega.tools

import mapreduce.ViquipediaParse.ResultViquipediaParsing
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.File
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.xml.{Elem, XML}

object ProcessFiles {

    case class ViquipediaFile(title: String, content: String, refs: List[String], file: File) {
        override def toString: String = s"ViquipediaFile(title: $title, filePath: $file, nº refs: ${refs.length})"
    }

    def readFile(file: String): String = {
        val initialFile = new File(file)
        val targetStream = FileUtils.openInputStream(initialFile)
        val textString = IOUtils.toString(targetStream, StandardCharsets.UTF_8)
        targetStream.close()
        textString
    }

    def getListOfFiles(dir: String): List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
            d.listFiles.filter(_.isFile).toList
        } else {
            List[File]()
        }
    }

    def showTxtFilesFromDirectory(directori: String): Unit = {
        val txtFiles = for (f <- getListOfFiles(directori); name = f.getName if name.takeRight(3) == "txt")
            yield name
        for (f <- txtFiles) {
            println(f)
            println(readFile(directori ++ "/" ++ f))
            println("--------------------------------------------------")
        }
    }

    def parseViquipediaFile(filename: String): ViquipediaFile = {
        val xmlleg = new java.io.InputStreamReader(new java.io.FileInputStream(filename), "UTF-8")

        // Agafo el document XML i ja està internament estructurat per anar accedint als camps que volguem
        val xmllegg: Elem = XML.load(xmlleg)

        // obtinc el titol
        val titol = (xmllegg \\ "title").text.toLowerCase()

        // obtinc el contingut de la pàgina
        val contingut = (xmllegg \\ "text").text.toLowerCase()

        // identifico referències
        val ref = new Regex("\\[\\[[^\\]]*\\]\\]")
        //println("La pagina es: " + titol)
        //println("i el contingut: ")
        //println(contingut)
        val refs = (ref findAllIn contingut).toList

        val filteredRefs = refs.filterNot(_.contains(':'))
            .map(_.replaceAll("\\[\\[|\\]\\]", ""))
            .filterNot(_.startsWith("#"))
            .map {
                ref =>
                    var res = ref
                    if (res.contains('|'))
                        res = res.split('|').head
                    if (res.contains('#'))
                        res = res.split('#').head

                    res
            }
            .distinct
            .filter(_ != titol)

        xmlleg.close()
        ViquipediaFile(titol, contingut, filteredRefs, new File(filename))
    }

    def loadCatalanStopWords():Unit = {
        //TODO
    }
}
