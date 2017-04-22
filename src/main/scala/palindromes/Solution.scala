package palindromes

object Solution {
  def solution(s: String): Int = {
    val count = new FunctionalPalindromicSlices(s).count()
    if(count <= 100000000) {
      count.toInt
    } else {
      -1
    }
  }
}
