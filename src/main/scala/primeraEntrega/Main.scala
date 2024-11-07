import pda.practica.DocumentSimilarity
import pda.practica.DocumentSimilarity.cosineSim

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Main extends App {
    private def generateSimilarityMatrix(txtFiles: List[Path], n: Int): Unit = {

        val fileNames = txtFiles.map(_.getFileName.toString)

        // Find the length of the longest file name to determine the column width
        val maxLength = fileNames.map(_.length).max + 2

        // Print the header row with file names, ensuring all file names fit within the column width
        printf("%-" + maxLength + "s", "")  // The top-left cell remains empty
        fileNames.foreach(file => if (file != "english-stop.txt") printf("%-" + maxLength + "s", file))  // Print file names with consistent width
        println()  // Move to the next line after the header

        // For each file (row), calculate similarities with all other files (including itself)
        for (i <- txtFiles.indices) {
            if (fileNames(i) != "english-stop.txt") {
                // Print the row header (file name) with consistent column width
                printf("%-" + maxLength + "s  ", fileNames(i))

                // For each file (column), compute the similarity score
                for (j <- txtFiles.indices) {
                    if (fileNames(j) != "english-stop.txt") {
                        // Compare files i and j
                        val similarity = DocumentSimilarity.cosineSim(Source.fromFile(txtFiles(i).toFile, StandardCharsets.UTF_8.name).mkString,
                            Source.fromFile(txtFiles(j).toFile, StandardCharsets.UTF_8.name).mkString,
                            n, stopwords)

                        // Print the similarity value with consistent width for all cells
                        printf("%-" + maxLength + ".3f", similarity)
                    }
                }

                println()  // Move to the next line after each row
            }
        }
    }

    println(DocumentSimilarity.filterWords("hola! ¿Que tal? Quin; exemple!mes;xulo.11 anys@").mkString("Array(", ", ", ")"))
    DocumentSimilarity.freq("hola hola adeu adeu adeu vinga vinga jaja jeje jiji")
    DocumentSimilarity.nonstopfreq("hola hola adeu adeu adeu vinga vinga jaja jeje jiji", List("hola", "jaja"))
    DocumentSimilarity.paraulesfreqfreq("hola hola adeu adeu adeu vinga vinga jaja jeje jiji")
    DocumentSimilarity.ngrames(3, "hola hola adeu hola hola adeu vinga jaja jeje jiji")

    println(DocumentSimilarity.cosineSim("hola", "hola", 1))
    println(DocumentSimilarity.cosineSim("hola", "adeu", 1))
    println(DocumentSimilarity.cosineSim("hola adeu", "adeu hola", 1))
    println(DocumentSimilarity.cosineSim("hola adeu", "adeu hola", 2))
    println(DocumentSimilarity.cosineSim("hola hola hola adeu", "hola hola adeu", 1))
    println(DocumentSimilarity.cosineSim("hola hola hola adeu", "hola hola adeu", 2))
    println(DocumentSimilarity.cosineSim("holap", "holak", 1))
    println(DocumentSimilarity.cosineSim("hola a b c d e", "hola f g h i j k l m n o p q", 1))
    println(DocumentSimilarity.cosineSim("hola a b c d e", "hola f g h i j k l m n o p q", 3))

    val currentPath = System.getProperty("user.dir")
    val directoryPath = currentPath + File.separator + "txt"

    println(s"Directory path: $directoryPath")

    val textFiles = Files.list(Paths.get(directoryPath)).iterator().asScala
        .filter(_.toString.endsWith(".txt")).toList

    val documents = textFiles.map { path =>
        val content = Source.fromFile(path.toFile, StandardCharsets.UTF_8.name).mkString
        (path.getFileName.toString, content)
    }.toMap

    documents.keys.foreach(println)

    println()

    val alice = documents.getOrElse("pg11.txt", "")
    val stopwords = documents.getOrElse("english-stop.txt", "").split("\\s+").filter(_.nonEmpty).toList
    val alice2 = documents.getOrElse("pg12.txt", "")

    DocumentSimilarity.freq(alice)
    println()
    DocumentSimilarity.nonstopfreq(alice, stopwords)
    println()
    DocumentSimilarity.paraulesfreqfreq(alice)
    println()
    DocumentSimilarity.ngrames(3, alice)
    println()
    println(DocumentSimilarity.cosineSim(alice, alice2, 1, stopwords))

    println("======================")
    generateSimilarityMatrix(textFiles, 1)
    println("======================")
    generateSimilarityMatrix(textFiles, 2)
    println("======================")
    generateSimilarityMatrix(textFiles, 3)
    println("======================")
}
