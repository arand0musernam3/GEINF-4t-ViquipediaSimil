package segonaEntrega.tools

import mapreduce.ViquipediaParse.ResultViquipediaParsing
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.File
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.xml.{Elem, XML}
import scala.io.Source

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

    def aux(file: ViquipediaFile): ViquipediaFile = {
        val titol = file.title
        val contingut = file.content

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

        val filteredContingut = if (true) {

            val wikiRegex = "\\[\\[(.*?)\\]\\]".r
            val templateRegex = "\\{\\{(.*?)\\}\\}".r

            val cleanedContent = wikiRegex.replaceAllIn(contingut, m => {
                val innerText = m.group(1)
                val parts = innerText.split("[|#]").map(_.trim)
                parts.lastOption.getOrElse("")
            })

            templateRegex.replaceAllIn(cleanedContent, m => {
                val innerText = m.group(1)
                val parts = innerText.split("[|#]").map(_.trim)
                parts.lastOption.getOrElse("")
            })

        } else {
            contingut
        }

        println(filteredContingut)

        ViquipediaFile(titol, filteredContingut, filteredRefs, null)
    }

    def parseViquipediaFile(filename: String, processContent: Boolean = false): ViquipediaFile = {
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

        val filteredContingut = if (processContent) {

            val wikiRegex = "\\[\\[(.*?)\\]\\]".r
            val templateRegex = "\\{\\{(.*?)\\}\\}".r

            var cleanedContent = wikiRegex.replaceAllIn(contingut, _ => "")

            cleanedContent = templateRegex.replaceAllIn(cleanedContent, _ => "")

            cleanedContent.split("\\s+").filterNot(catalanStopwords.contains).mkString(" ")
        } else {
            contingut
        }

        xmlleg.close()
        ViquipediaFile(titol, filteredContingut, filteredRefs, new File(filename))
    }

    private def loadCatalanStopWords() = {
        Source.fromFile("stopwordscatalanet.txt", StandardCharsets.UTF_8.name).mkString.split("\\s+").filter(_.nonEmpty).toSet
    }

    private val catalanStopwords: Set[String] = loadCatalanStopWords()
}
