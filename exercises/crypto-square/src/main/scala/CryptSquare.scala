
object CryptoSquare {

  def squareCols(size: Int): Int =
    Math.ceil(Math.sqrt(size)).toInt

  def ciphertext(text: String): String = {
    if (text.isEmpty)
      ""
    else {
      val normalized =
        text
          .filterNot(_.isWhitespace)    // [...] the spaces
          .filter(_.isLetterOrDigit)    // and punctuation are removed [...]
          .toLowerCase                  // and the message is downcased.

      val size = normalized.length
      val cols = squareCols(size)
      val rows =
        if (cols * (cols - 1) > size) cols - 1
        else cols

      val response =
        normalized
          .grouped(cols)                // [...]  are broken into rows (of cols length)
          .foldLeft(List.fill(rows)("")){(acc, elem) => // transpose the rectangle
             acc
               .zipAll(elem, "", " ")
               .map{case (a, e) => a + e}
           }
          .mkString(" ")

      response
    }
  }

}
