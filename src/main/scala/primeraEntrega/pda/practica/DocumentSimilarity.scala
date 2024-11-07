package pda.practica

object DocumentSimilarity {

  def simil(text1: String, text2: String, stopWords: List[String], n:Int): Double = {
    cosineSim(text1, text2, 1, stopWords)
  }

  private def printFirstNFreq(list: List[(String, Int)]): Unit = {
    val totalWordCount = list.map(_._2).sum
    printf("Num de Paraules:\t%d\t\t\t\tDiferents:\t%d\n", totalWordCount, list.length)
    printf("Paraules\t\t\tOcurrencies\t\t\tFrequencia\n")
    println("------------------------------------------------------------")
    for (i <- list.take(10)) {
      printf("%-" + 20 + "s%-" + 20 + "d%.2f\n", i._1, i._2, (i._2.toDouble / totalWordCount) * 100)
    }
  }

  def filterWords(string: String): Array[String] = {
    string
      .toLowerCase
      .replaceAll("[-_'.,;:()!?¿¡\\[\\]{}1234567890$%&/\\\\@|#ºª+*^<>€=\\n\\t\\r]", " ")
      .split("\\W+")
      .filter(_.nonEmpty)
  }

  def freq(string: String, print: Boolean = true): List[(String, Int)] = {
    val aux = filterWords(string)
      .groupBy(identity)
      .view.mapValues(_.length)
      .toList
      .sortBy(-_._2) //ordena descendentment pel segon element de la tupla

    if (print)
      printFirstNFreq(aux)

    aux
  }

  def nonstopfreq(string: String, stopWords: List[String], print: Boolean = true): List[(String, Int)] = {
    val aux = freq(string, print = false)
      .filter { case (w, _) => !stopWords.contains(w) }

    if (print)
      printFirstNFreq(aux)

    aux
  }

  def paraulesfreqfreq(string: String, print: Boolean = true): Map[Int, Int] = {
    val aux = freq(string, false)

    val freqCounts = aux
      .groupBy(_._2)
      .view.mapValues(_.length).toMap
      .toSeq
      .sortBy(_._2)

    if (print) {
      println("\nLes 10 frequencies mes frequents:")
      freqCounts.takeRight(10).reverse.foreach {
        case (freq, count) =>
          printf("%d paraules apareixen %d vegades\n", count, freq)
      }
      println()
      println("Les 5 frequencies menys frequents:")
      freqCounts.take(5).foreach {
        case (freq, count) =>
          printf("%d paraules apareixen %d vegades\n", count, freq)
      }
    }

    freqCounts.toMap
  }

  def ngrames(n: Int, string: String, print: Boolean = true): Seq[(String, Int)] = {
    val aux = filterWords(string)
      .sliding(n)
      .map(_.mkString(" "))
      .toSeq
      .groupBy(identity)
      .view.mapValues(_.length)
      .toSeq
      .sortBy(-_._2)

    if (print) {
      aux.take(10).foreach { case (string, count) =>
        printf("%-" + 40 + "s%d\n", string, count)
      }
    }

    aux.toList
  }

  private def normalizedFrequencies(words: Seq[(String, Int)]): Map[String, Double] = {
    val wordCounts = words.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
    val maxFreq = wordCounts.values.max.toDouble

    wordCounts.map { case (word, freq) => (word, freq / maxFreq) }
  }

  def cosineSim(text1: String, text2: String, n: Int, stopWords: List[String] = List()): Double = {
    // Freq
    val freq1 = if (n == 1) nonstopfreq(text1, stopWords, print = false) else ngrames(n, text1, print = false)
    val freq2 = if (n == 1) nonstopfreq(text2, stopWords, print = false) else ngrames(n, text2, print = false)

    // Frequency normalization
    val normalizedFreq1 = normalizedFrequencies(freq1)
    val normalizedFreq2 = normalizedFrequencies(freq2)

    // Unified set
    val allWords = (normalizedFreq1.keySet ++ normalizedFreq2.keySet).toList

    // Aligned vectors generation
    val vector1 = allWords.map(word => normalizedFreq1.getOrElse(word, 0.0))
    val vector2 = allWords.map(word => normalizedFreq2.getOrElse(word, 0.0))

    // Scalar product and vector magnitudes
    val dotProduct = vector1.zip(vector2).map { case (a, b) => a * b }.sum
    val magnitude1 = math.sqrt(vector1.map(a => a * a).sum)
    val magnitude2 = math.sqrt(vector2.map(b => b * b).sum)

    if (magnitude1 == 0 || magnitude2 == 0) 0.0
    else dotProduct / (magnitude1 * magnitude2)
  }
}


