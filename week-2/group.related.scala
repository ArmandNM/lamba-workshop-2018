import scala.collection.immutable.Queue

case class State(resultQ: Queue[String], rest: String)

val inputString = "Hello world"

val initialState = State(Queue.empty[String], inputString)

def transition(s: State): State = {
  val remHeadOption = s.rest.headOption
  remHeadOption.map { case remHead =>
    val (fst, snd) = s.rest.span({case c => c == remHead})
    val nextResultQ = s.resultQ.enqueue(fst)
    State(nextResultQ, snd)
  }.getOrElse(s)}

val solution = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, rest) => rest.nonEmpty }
    .head
    .resultQ
    .toList
}
