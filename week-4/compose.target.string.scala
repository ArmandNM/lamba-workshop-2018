case class State(remainingString: String, dp: List[Boolean])
case class Accumulator(prefix: List[Char], revDP: List[Boolean])

val stringsSet: Set[String] = Set("one", "cat", "two", "four")
val targetString: String = "fouroneone"

val initialState: State = State(targetString, )

def foldFun(acc: Accumulator, cValue: Char) {
  newPrefix = cValue :: acc.prefix
  if (stringsSet.contains(newPrefix)) {
    Accumulator(newPrefix, true::acc.revDP)
  } else {
    Accumulator(newPrefix, false::acc.revDP)
  }
}

def 
