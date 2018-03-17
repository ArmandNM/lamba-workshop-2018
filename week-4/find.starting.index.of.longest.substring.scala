import scala.collection.immutable.Queue

case class State(sequence: String, crtIndex: Int, startingIndex: Int, lengthOfLongestSequence: Int)

val inputString = "abcccdeeee"

val initialState = State(inputString, 0, 0, 0)

def transition(s: State): State = {
  val remHeadOption = s.sequence.headOption
  remHeadOption.map { case remHead =>
    val (fst, snd) = s.sequence.span({case c => c == remHead})

    if (fst.length > s.lengthOfLongestSequence) {
      State(snd, s.crtIndex + fst.length, s.crtIndex, fst.length)
    } else {
      State(snd, s.crtIndex + fst.length, s.startingIndex, s.lengthOfLongestSequence)
    }
  }.getOrElse(s)}

val solution = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(sequence, _, _, _) => sequence.nonEmpty }
    .head
    .startingIndex
}
