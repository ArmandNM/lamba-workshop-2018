def numOfOnesHeadRecursion(number: Int): Int = {
  if (number != 0) {
    1 + numOfOnesHeadRecursion(number & (number - 1))
  } else 0
}

def numOfOnesTailRecursion(number: Int, ones: Int): Int = {
  if (number != 0) {
    numOfOnesTailRecursion(number & (number - 1), ones + 1)
  } else {
    ones
  }
}

numOfOnesHeadRecursion(143213)
numOfOnesTailRecursion(143213, 0)
