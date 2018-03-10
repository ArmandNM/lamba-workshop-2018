import scala.util.Random.nextInt

case class State(list: List[Int], length: Int, k: Int)

def transition(s: State): State = {
  val pivotIndex = nextInt(s.length)
  val pivotValue = s.list(pivotIndex)

  val (leftPartition, rightPartition) = s.list.partition { case i => i < pivotValue}
  val leftPartitionLength = leftPartition.length
  val rightPartitionLength = rightPartition.length

  if (s.k  >= leftPartitionLength) {
    State(rightPartition, rightPartitionLength, s.k-leftPartitionLength)
  } else {
    State(leftPartition, leftPartitionLength, s.k)
  }
}

val inputList = List(1, 7, 4, 5, 13, 101)
val initialState = State(inputList, inputList.length, 3)

Get the result after iterating the whole list
val kthElement = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, _, k) => k > 0 }
    .head
    .list
    .head
}
