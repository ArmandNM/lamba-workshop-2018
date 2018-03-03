import scala.collection.immutable.Queue

case class State(resultQ: Queue[Int], elemC: Option[Int], rest: List[Int])

val lst = List(-7, 1, 5, 2, -4, 3, 0)

val initialState = State(Queue.empty[Int], lst.headOption, lst.tail)

def transition(s: State): State = {
  val restHeadOption = s.rest.headOption
  restHeadOption.map { case restHead =>
    val nextRest = s.rest.tail
    val min = scala.math.min(restHead, s.elemC.get)
    val max = scala.math.max(restHead, s.elemC.get)
    State(s.resultQ.enqueue(min), Some(max), nextRest)
  }.getOrElse {
    val nextResultQ = s.elemC.map { case el => s.resultQ.enqueue(el) }.getOrElse(s.resultQ)
    State(nextResultQ, None, List.empty[Int])
  }
}

val afterFirstBubble = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, _, lst) => lst.nonEmpty }
    .tail
    .head
    .resultQ
    .toList
}
