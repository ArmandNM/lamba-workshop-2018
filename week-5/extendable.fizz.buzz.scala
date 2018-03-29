case class State(crtAnswer: List[String], listOfStreamedTokens: List[Stream[String]])

def transition(s: State): State = {
  val listofHeads = s.listOfStreamedTokens.map { case stream => stream.head }
  val concatenatedHeads = listofHeads.reduce(_ + _)
  val newAnswer = concatenatedHeads :: s.crtAnswer
  val listOfStreamsWithoutHead = s.listOfStreamedTokens.map { case stream => stream.drop(1) }

  State(newAnswer, listOfStreamsWithoutHead)
}

def tokenToStream(index: Int, token: String): Stream[String] = {
  // Create an initial list containing only empty strings
  val listEmptyString = List.fill(index - 1){""}
  // Add the token at the end of the list
  val listWithToken = listEmptyString ::: List(token)
  // Convert it to a repeating stream
  lazy val repeatingStream: Stream[String] = listWithToken.toStream.append(repeatingStream)
  // return the stream ^^
  repeatingStream
}

def fizzBuzz(steps: Int, tokens: List[(Int, String)]): List[String] = {
  val listOfStreamedTokens: List[Stream[String]] = tokens.map { case (index, token) => tokenToStream(index, token) }

  val initialState = State(List[String](), listOfStreamedTokens)

  Stream
    .iterate(initialState)(transition)
    .drop(steps)
    .head
    .crtAnswer
    .reverse
}
