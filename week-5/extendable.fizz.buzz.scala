def tokenToStream(token: String, index: Int): Stream[String] = {
  // Create an initial list containing only empty strings
  val listEmptyString = List.fill(index - 1){""}
  // Add the token at the end of the list
  val listWithToken = listEmptyString ::: List(token)
  // Convert it to a repeating stream
  lazy val repeatingStream: Stream[String] = listWithToken.toStream.append(repeatingStream)
  // return the stream ^^
  repeatingStream
}

def fizzBuzz(steps: Int, tokens: List[(String, Int)]): List[String] = {
  val listOfStreamedTokens: List[Stream[String]] = tokens.map { case (token, index) => tokenToStream(token, index) }
  val listOfZippedStreams = listOfStreamedTokens.transpose
  lazy val answer = listOfZippedStreams.map {case list => list.reduce(_ + _)}
  answer.take(steps).toList
}


def tts(): Stream[String] = {

}
