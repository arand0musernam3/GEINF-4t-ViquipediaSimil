package segonaEntrega.tools

import mapreduce.ViquipediaParse.ResultViquipediaParsing
import org.apache.commons.io.{FileUtils, IOUtils}
import segonaEntrega.firstSubmission.DocumentSimilarity

import java.io.File
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.xml.{Elem, XML}
import scala.io.Source

object ProcessFiles {

    case class ViquipediaFile(title: String, content: String, refs: List[String], file: File) {
        override def toString: String = s"ViquipediaFile(title: $title, filePath: $file, nº refs: ${refs.length})"
    }

    def getListOfFiles(dir: String): List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
            d.listFiles.filter(_.isFile).toList
        } else {
            List[File]()
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

        xmlleg.close()
        ViquipediaFile(titol, contingut, filteredRefs, new File(filename))
    }

    private def loadCatalanStopWords(): Set[String] = {
        Source.fromFile("stopwordscatalanet.txt", StandardCharsets.UTF_8.name).mkString.split("\\s+").filter(_.nonEmpty).toSet
    }

    def filterViquipediaFile(vf: ViquipediaFile) : ViquipediaFile = {
        val content = vf.content

        val wikiRegex = "\\[\\[([^\\[\\]]*:)[^\\[\\]]*\\]\\]".r //delete all references which contain a ":"
        val templateRegex = "\\{\\{(.*?)\\}\\}".r

        var cleanedContent = wikiRegex.replaceAllIn(content, _ => " ")
        cleanedContent = templateRegex.replaceAllIn(cleanedContent, _ => " ")
        cleanedContent = DocumentSimilarity.filterWords(cleanedContent)
            .filterNot(catalanStopwords.contains)
            .mkString(" ")


        ViquipediaFile(title = vf.title, content = cleanedContent, refs = vf.refs, file = vf.file)
    }

    private val catalanStopwords: Set[String] = loadCatalanStopWords()
}
