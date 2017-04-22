package palindromes

/***
  * Object with API required by Codility
  * https://codility.com/programmers/task/count_palindromic_slices/
  */
object Solution {
  def solution(s: String): Int = {
    val count = new FunctionalPalindromicSlices(s, doLogResults = false).count()
    if(count <= 100000000) {
      count.toInt
    } else {
      -1
    }
  }
}
