// K Largest Integers

import scala.collection.mutable.PriorityQueue

case class State(pq: PriorityQueue[Int], pqSize: Int)

val k: Int = 5

def foldLeftFun(acc: State, number: Int): State = {
  acc.pq.enqueue(number)
  val newPQSize = acc.pqSize + 1

  val resultState = if (newPQSize > k) {
    acc.pq.dequeue
    acc
  } else {
    acc.copy(pqSize = newPQSize)
  }

  resultState
}

val data: List[Int] = List(521, 162, 826, 523, 461, 421, 425, 170, 468, 957, 902, 467, 613, 177, 462, 376, 883, 217, 856, 119)

val initialState: State = State(PriorityQueue.empty[Int](Ordering.Int.reverse), 0)

// Get the top k largest integers
val topK = {
  data
    .foldLeft(initialState)(foldLeftFun)
    .pq
    .dequeueAll
    .toList
}

// The result should be List(826, 856, 883, 902, 957)
