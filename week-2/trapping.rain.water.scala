case class State(leftStack: MaxStack, rightStack: MaxStack, rem: List[Int], runningSum: Int)

val elevationList: List[Int] = List(0,1,0,2,1,0,1,3,2,1,2,1)
val reversedElevationList = elevationList.reverse
val emptyMaxStack = MaxStack(List.empty[StackItem])
val initialRightStack = reversedElevationList.foldLeft(emptyMaxStack){ case (accMaxStack, cValue) =>
  push(accMaxStack, cValue)
}

// [TODO] finish the problem
