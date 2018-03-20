case class State(remainingString: List[Char], dp: List[Boolean])
case class Accumulator(prefix: List[Char], revDP: List[Boolean])

val stringsSet: Set[List[Char]] = Set("one".toList, "cat".toList, "two".toList, "four".toList)
val targetString: List[Char] = "fouroneone".toList

val initialState: State = State(targetString.toList, List.fill(targetString.length){false})

def foldFun(acc: Accumulator, cValue: Char) = {
  val newPrefix = cValue :: acc.prefix
  if (stringsSet.contains(newPrefix)) {
    Accumulator(newPrefix, true::acc.revDP)
  } else {
    Accumulator(newPrefix, false::acc.revDP)
  }
}

def transition(s: State): State = {

}
