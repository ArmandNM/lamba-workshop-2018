case class State(leftStack: MaxStack, rightStack: MaxStack, rem: List[Int], runningSum: Int)

// prepare the state members
val elevationList: List[Int] = List(10,5,10)
val reversedElevationList = elevationList.reverse
val emptyMaxStack = MaxStack(List.empty[StackItem])
val initialRightStack = reversedElevationList.foldLeft(emptyMaxStack){ case (accMaxStack, cValue) =>
  push(accMaxStack, cValue)
}

val initialState = State(emptyMaxStack, initialRightStack, elevationList, 0)

def transition(s: State): State = {
  val restHeadOption = s.rem.headOption
  restHeadOption.map { case restHead =>
    val nextRest = s.rem.tail
    val leftMax = max(s.leftStack).getOrElse(0)
    val rightMax = max(s.rightStack).getOrElse(0)
    
    // Compute the water level at the current position
    val waterLevel = scala.math.min(leftMax, rightMax) - restHead
    val positiveWaterLevel = if (waterLevel > 0) {waterLevel} else {0}

    // Construct the new State
    val newRunningSum = s.runningSum + positiveWaterLevel
    val newLeftStack = push(s.leftStack, restHead)
    val (_, newRightStack) = pop(s.rightStack)
    State(newLeftStack, newRightStack, nextRest, newRunningSum)
  }.getOrElse {
    s
  }
}

// Get the result after iterating the whole list
val trappedWaterVolume = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, _, rem, _) => rem.nonEmpty }
    .head
    .runningSum
}
