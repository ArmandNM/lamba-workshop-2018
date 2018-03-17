case class State(list: List[Double], crtMin: Double, crtMax: Double)

val inputList: List[Double] = List(1,2,3,4,5)
val initialState = State(inputList.tail, inputList.head, inputList.head)

def transition(s: State): State = {
  val remHeadOption = s.list.headOption
  remHeadOption.map { case remHead =>
    val sortedValues = List(
      s.crtMin + remHead, s.crtMax + remHead,
      s.crtMin - remHead, s.crtMax - remHead,
      s.crtMin * remHead, s.crtMax * remHead,
      s.crtMin / remHead, s.crtMax / remHead
    ).sorted

    val newMax = sortedValues.last
    val newMin = sortedValues.head
    val rest = s.list.tail

    State(rest, newMin, newMax)
  }.getOrElse(s)
}

val solution = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(list, _, _) => list.nonEmpty }
    .head
    .crtMax
}
