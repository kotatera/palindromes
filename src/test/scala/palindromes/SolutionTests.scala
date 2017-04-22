package palindromes

import org.scalatest.FunSuite

class SolutionTests extends FunSuite {

  test("Empty string has no palindromic slices") {
    assert(Solution.solution("") == 0)
  }

  test("For more than 100000000 palindrome slices should return -1") {
    val input = List.fill(20000){'b'}.mkString
    assert(Solution.solution(input) == -1)
  }

  test("For some known cases palindromic slices should be calculated properly") {
    Map(
      "aabbaa" -> 5,
      "abcacbbbca" -> 7,
      "kobylamamalybok" -> 9,
      "anetaatena" -> 5,
      "baababa" -> 6
    ).foreach(i => {
      assert(Solution.solution(i._1) == i._2)
    })
  }
}