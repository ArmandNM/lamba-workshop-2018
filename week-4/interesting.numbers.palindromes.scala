case class State(remainingNumbers: List[Int], )

def isPalindrome(number: Int): Boolean = {
  val reversedNumber = number.toString.reverse.toInt
  number == reversedNumber
}

def from(start: Int): Stream[Int] =
start #:: from(start + 1)

val naturals = from(0)

val interestingNumbers = naturals.filter { case x =>
  val revX = x.toString.reverse.toInt
  val sum = x + revX
  sum > 1000 && isPalindrome(sum)
}.take(25).toList
