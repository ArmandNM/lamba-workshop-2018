case class State(leftStack: MaxStack, rightStack: MaxStack, runningSum: Int)

// prepare the state members
val elevationList: List[Int] = List(10,5,10)
val reversedElevationList = elevationList.reverse
val emptyMaxStack = MaxStack(List.empty[StackItem])
val initialRightStack = reversedElevationList.foldLeft(emptyMaxStack){ case (accMaxStack, cValue) =>
  push(accMaxStack, cValue)
}

val initialState = State(emptyMaxStack, initialRightStack, 0)

def foldLeftFun(acc: State, elevation: Int): State = {
  val leftMax = max(acc.leftStack).getOrElse(0)
  val rightMax = max(acc.rightStack).getOrElse(0)

  // Compute the water level at the current position
  val waterLevel = scala.math.min(leftMax, rightMax) - elevation
  val positiveWaterLevel = if (waterLevel > 0) {waterLevel} else {0}

  // Construct the new State
  val newRunningSum = acc.runningSum + positiveWaterLevel
  val newLeftStack = push(acc.leftStack, elevation)
  val (_, newRightStack) = pop(acc.rightStack)

  State(newLeftStack, newRightStack, newRunningSum)
}

// Get the result after iterating the whole list
val trappedWaterVolume = {
  elevationList
    .foldLeft(initialState)(foldLeftFun)
    .runningSum
}
