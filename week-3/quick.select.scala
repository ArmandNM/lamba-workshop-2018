import scala.util.Random.nextInt

case class State(list: List[Int], length: Int, k: Int)

def transition(s: State): State = {
  val pivotIndex = nextInt(s.length)
  val pivotValue = s.list(pivotIndex)

  val (leftPartition, rightPartition) = s.list.partition { case i => i < pivotValue}
  val leftPartitionLength = leftPartition.length
  val rightPartitionLength = rightPartition.length

  if (leftPartitionLength == 0 && rightPartition.distinct.length == 1) {
    State(rightPartition.distinct, 1, 1)
  } else if (s.k  > leftPartitionLength) {
    State(rightPartition, rightPartitionLength, s.k-leftPartitionLength)
  } else {
    State(leftPartition, leftPartitionLength, s.k)
  }
}

val inputList = List(99, 99)
val initialState = State(inputList, inputList.length, 1)

// Get the result after iterating the whole list
val kthElement = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, length, _) => length > 1 }
    .head
    .list
    .head
}
