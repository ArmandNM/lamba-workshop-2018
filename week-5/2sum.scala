case class Accumulator(values: Map[Int, Int], crtIndex: Int, pair: List[Int])

val targetSum = 6;
val inputValues = List(3,2,4)
val initialAcc = Accumulator(Map[Int, Int](), 0, List[Int]())

def foldLeftFun(targetSum: Int)(acc: Accumulator, crtValue: Int) = {
  val remainder = targetSum - crtValue
  if (acc.values.contains(remainder)) {
    Accumulator(acc.values ++ Map[Int, Int](crtValue->acc.crtIndex), acc.crtIndex + 1, acc.values.getOrElse(remainder, -1) :: acc.crtIndex :: acc.pair)
  } else {
    Accumulator(acc.values ++ Map[Int, Int](crtValue->acc.crtIndex), acc.crtIndex + 1, acc.pair)
  }
}

val answer = {
  inputValues
    .foldLeft(initialAcc)(foldLeftFun(targetSum))
    .pair
}
