package palindromes

import scala.annotation.tailrec

/***
  * Looking for palindromic slices of a given input string
  * It is the same algorithm as in @ImperativePalindromicSlices but this is written in more functional-style
  * @param input string with palindromic slices to be found
  * @param doLogResults boolean flag to indicate if it should print palindrome slices found on STDOUT
  */
class FunctionalPalindromicSlices(input: String, doLogResults: Boolean = true) extends PalindromicSlices {
  val guardedInput = s"@$input#"

  override def count(): Long = {
    val odds = lookForPalindromes(Odd)
    val evens = lookForPalindromes(Even)
    val all = odds ++ evens

    if(doLogResults) { printPalindromes(all) }

    all.map(_.radius).sum
  }

  private trait PalindromeType { val mirrorOffset: Int }
  private case object Even extends PalindromeType { override val mirrorOffset = 1 }
  private case object Odd extends PalindromeType { override val mirrorOffset = 0 }

  private case class Palindrome(offset: Int, radius: Long, pType: PalindromeType)


  private def lookForPalindromes(palindromeType: PalindromeType): List[Palindrome] = {
    val n = input.length

    @tailrec
    def go(radius: Int, index: Int, currentPalindromes: Map[Int, Palindrome]): Map[Int, Palindrome] = {
      if(index <= n) {
        val newRadius = radiusForIndex(radius, index, palindromeType)
        val newPalindromes = if(newRadius <= 0) { currentPalindromes }
          else { currentPalindromes + (index -> Palindrome(index - 1, newRadius, palindromeType)) }

        val (checked, includingSubpalindromes) = markSubpalindromes(index, newRadius, newPalindromes)

        go(0 max (newRadius - checked), index + checked, includingSubpalindromes)
      } else {
        currentPalindromes
      }
    }

    go(0, 1, Map()).values.toList
  }

  @tailrec
  private def radiusForIndex(radius: Int, index: Int, palindromeType: PalindromeType): Int = {
    val leftIndex = index - radius - 1
    val rightIndex = index + radius + palindromeType.mirrorOffset
    if(guardedInput.charAt(leftIndex) == guardedInput.charAt(rightIndex))
      radiusForIndex(radius + 1, index, palindromeType)
    else
      radius
  }

  private def markSubpalindromes(index: Int, radius: Int, previousPalindromes: Map[Int, Palindrome]): (Int, Map[Int, Palindrome]) = {

    @tailrec
    def go(offset: Int, currentPalindromes: Map[Int, Palindrome]): (Int, Map[Int, Palindrome]) = {
      currentPalindromes.get(index - offset) match {
        // not within the radius
        case _ if offset >= radius => (offset, currentPalindromes)

        // this palindrome seems to be a subpalindrome of some even larger palindrome
        case None if 0 == radius - offset => (offset, currentPalindromes)
        case Some(p) if p.radius == radius - offset => (offset, currentPalindromes)

        // no subpalindrome
        case None => go(offset + 1, currentPalindromes)

        // found subpalindrome, copy to the right side of the index
        case Some(p) if p.radius != radius - offset => {
          val spRadius = p.radius min (radius - offset)
          go(offset + 1, currentPalindromes + (index + offset -> Palindrome(index + offset - 1, spRadius, p.pType)))
        }
      }
    }

    go(1, previousPalindromes)
  }

  private def printAll(palindromes: Iterable[Palindrome]) = {
    println(s"$input")
    palindromes.foreach(p => {
      for(r <- p.radius to 1 by -1) {
        print(List.fill(p.offset - r.toInt){' '}.mkString)
        println(input.substring(p.offset - r.toInt, p.offset + r.toInt + p.pType.mirrorOffset))
      }
    })
  }

  private def printPalindromes(palindromes: Iterable[Palindrome]) = input.length match {
    case short if short <= 50 => {
      printAll(palindromes)
    }
    case longer if longer > 50 && longer <= 120 => {
      println(s"Input string is pretty long, it has $longer characters. Printing only first 10 palindromes")
      printAll(palindromes.take(10))
    }
    case wayTooLong => {
      println(s"Input string is way too long - it has $wayTooLong characters. " +
        s"I'm not printing it at all - you would not see anything anyway")
    }
  }

}
