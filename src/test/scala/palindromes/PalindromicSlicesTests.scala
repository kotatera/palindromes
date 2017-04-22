package palindromes

import org.scalatest.FunSuite

import scala.util.Random

trait TestCases[T <: PalindromicSlices] extends FunSuite {
  def uut(input: String): T

  test("Empty string has no palindromic slices") {
    assert(uut("").count() == 0)
  }

  test("Single-character string has no palindromic slices") {
    assert(uut("a").count() == 0)
  }

  test("Max number of palindrome slices for input of max lenght") {
    val input = List.fill(20000){'b'}.mkString
    assert(uut(input).count() == 199990000)
  }

  test("For a sequence of the same letter result should be the sum of numbers since 1 till n - 1") {
    Seq(2, 3, 4, 10, 33, 10000, 19999, 20000).foreach(n => {
      val luckyChar: Char = Random.alphanumeric.take(1).mkString.charAt(0)
      val input = List.fill(n){luckyChar}.mkString
      assert(uut(input).count() == n * (n-1) / 2)
    })
  }

  test("Sequence of char A followed by sequence of B should result in sum(1..An-1) + sum(1..Bn-1)") {
    Seq(2, 3, 4, 10, 33, 5000, 9999, 10000).foreach(n => {
      val input = List.fill(n){'A'}.mkString + List.fill(n){'B'}.mkString
      assert(uut(input).count() == n * (n-1) / 2 + n * (n-1) / 2)
    })
  }

  test("For some known cases palindromic slices should be calculated properly") {
    Map(
      "aabbaa" -> 5,
      "abcacbbbca" -> 7,
      "kobylamamalybok" -> 9,
      "anetaatena" -> 5,
      "baababa" -> 6
    ).foreach(i => {
      assert(uut(i._1).count() == i._2)
    })
  }
}

class FunctionalPalindromicSlicesTests extends TestCases[FunctionalPalindromicSlices] {
  override def uut(input: String): FunctionalPalindromicSlices = new FunctionalPalindromicSlices(input)
}

class ImperativePalindromicSlicesTests extends TestCases[ImperativePalindromicSlices] {
  override def uut(input: String): ImperativePalindromicSlices = new ImperativePalindromicSlices(input)
}
