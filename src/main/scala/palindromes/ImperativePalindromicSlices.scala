package palindromes

import scala.collection.mutable

/***
  * Looking for palindromic slices of a given input string
  * Rewritten to Scala based on http://codepad.org/vAGMSNKG (almost 'as-is', i.e. in imperative style)
  * @param input string with palindromic slices to be found
  */
class ImperativePalindromicSlices(input: String) extends PalindromicSlices {
  private val guarded = s"@$input#"
  private val n = input.length

  override def count(): Long = {
    val odds = mutable.ArrayBuffer.fill(n + 1) { 0 }
    val evens = mutable.ArrayBuffer.fill(n + 1) { 0 }

    markPalindromes(odds, 0)
    markPalindromes(evens, 1)

    printResults(odds, evens)

    odds.sum + evens.sum // value of radius is equal to number of sub-palindromes with the center on particular index
  }

  private def markPalindromes(palindromeMarks: mutable.ArrayBuffer[Int], mirrorOffset: Int) = {
    var radius = 0
    var i = 1
    while (i <= n) {
      while (guarded.charAt(i - radius - 1) == guarded.charAt(i + radius + mirrorOffset)) {
        radius = radius + 1
      }
      palindromeMarks(i) = radius

      var j = 1
      while (palindromeMarks(i - j) != radius - j && j < radius) {
        palindromeMarks(i + j) = palindromeMarks(i - j).min(radius - j)
        j = j + 1
      }
      radius = 0 max (radius - j)
      i = i + j
    }
  }

  private def printResults(odds: mutable.ArrayBuffer[Int], evens: mutable.ArrayBuffer[Int]) = {
    if(n < 50) {
      println(s" $input")
      for (i <- 1 to n) {
        for (r <- odds(i) to 0 by -1) {
          if (r > 0) {
            for (m <- 1 to i - r) print(" ")
            println(input.substring(i - r - 1, i + r - 1))
          }
        }

        for (r <- evens(i) to 0 by -1) {
          if (r > 0) {
            for (m <- 1 to i - r) print(" ")
            println(input.substring(i - r - 1, i + r))
          }
        }

      }
    } else {
      println("Too long input to print")
    }
  }

}
