object Solution {
  case class Acc(fishStack: List[Int], numOfFishAlive: Int)
  case class FightingState(fishStack: List[Int], fishSize: Option[Int])

  def afterFighting(fs: FightingState): FightingState = {
    def transition(fs: FightingState): FightingState = {
      (fs.fishStack.headOption, fs.fishSize) match {
        case (Some(fishStackHead), Some(fishSize)) =>
          if (fishStackHead > fishSize) {
            FightingState(fs.fishStack, None)
          } else {
            FightingState(fs.fishStack.tail, fs.fishSize)
          }
        case _ => fs
      }
    }

    Stream
      .iterate(fs)(transition)
      .dropWhile {
        case FightingState(fishStack, fishSize) =>
          fishStack.nonEmpty && fishSize.isDefined
      }
      .head
  }

  def solution(a: Array[Int], b: Array[Int]): Int = {
    val fishDescription = a.zip(b)
    val initialAcc = Acc(List[Int](), 0)

    def foldLeftFn(acc: Acc, crtFishDescription: (Int, Int)): Acc = {
      val (crtFishSize, crtOrientation) = crtFishDescription

      if (crtOrientation == 1) {
        Acc(crtFishSize :: acc.fishStack, acc.numOfFishAlive)
      } else {
        val initialFightingState = FightingState(acc.fishStack, Some(crtFishSize))
        val afterFightingState = afterFighting(initialFightingState)
        if (afterFightingState.fishStack.isEmpty) {
          Acc(afterFightingState.fishStack, acc.numOfFishAlive + 1)
        } else {
          Acc(afterFightingState.fishStack, acc.numOfFishAlive)
        }
      }
    }

    val finalAcc = fishDescription.foldLeft(initialAcc)(foldLeftFn)
    val totalFishAlive = finalAcc.fishStack.length + finalAcc.numOfFishAlive

    totalFishAlive
  }
}
