case class State(remainingString: List[Char], dp: List[Boolean])
case class Accumulator(prefix: List[Char], revDP: List[Boolean])

val stringsSet: Set[List[Char]] =
  Set(
    "one".toList.reverse,
    "cat".toList.reverse,
    "two".toList.reverse,
    "four".toList.reverse
  )

val targetString: List[Char] = "fouroneonex".toList

def foldLeftFun(acc: Accumulator, cValue: Char): Accumulator = {
  val newPrefix = cValue :: acc.prefix
  if (stringsSet.contains(newPrefix)) {
    Accumulator(newPrefix, true::acc.revDP)
  } else {
    Accumulator(newPrefix, false::acc.revDP)
  }
}

val emptyAcc = Accumulator(List[Char](), List[Boolean]())
val initialDP = targetString.foldLeft(emptyAcc)(foldLeftFun).revDP.reverse
val initialState: State = State(targetString.toList, initialDP)

def transition(s: State): State = {
  val (fst, snd) = s.dp.drop(s.dp.length - s.remainingString.length) span {case d => d == false}
  val sndPartOfRemainingString = s.remainingString.drop(fst.length + 1)
  val sndDP = sndPartOfRemainingString.foldLeft(emptyAcc)(foldLeftFun).revDP.reverse
  val newDP = List.fill(s.dp.length - s.remainingString.length + fst.length){false} ::: List(true) ::: sndDP
  val reunitedDP = (s.dp, newDP).zipped.map {_ || _}
  State(sndPartOfRemainingString, reunitedDP)
}

// Final DP solution for all substrings
val finalDP = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(remainingString, _) => remainingString.nonEmpty }
    .head
    .dp
}

val isPossible = finalDP.last == true
