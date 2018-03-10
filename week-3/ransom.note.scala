val newspaperText = "in while attending the university of helsinki torvalds became curious about operating systems frustrated by the licensing of minix which at the time limited it to educational use only he began to work on his own operating system kernel which eventually became the linux kernel".replaceAll("[^a-z]", "")
val targetText = "helloworld"

def countCharacters(str: String): List[Int] = {
  val mutarr = Array.fill[Int](26)(0)
  str.foreach { case i => mutarr(i - 'a') += 1 }
  mutarr.toList
}

val newspaperCharFrequency = countCharacters(newspaperText)
val targetCharFrequency = countCharacters(targetText)

val diffCharFrequency = {
  newspaperCharFrequency
    .zip(targetCharFrequency
    .map { case (first, second) => first - second}
}

val isPossible = diffCharFrequency.find {case i => i < 0}.isEmpty
