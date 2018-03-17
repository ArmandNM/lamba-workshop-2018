import scala.collection.Searching._

val lookedUp: Long = 845L

val matr: Vector[Vector[Long]] =
  Vector(Vector(148, 185, 238, 256, 291, 314, 324),
         Vector(350, 356, 367, 382, 383, 414, 418),
         Vector(432, 446, 507, 511, 527, 540, 552),
         Vector(553, 556, 592, 607, 610, 632, 634),
         Vector(661, 675, 691, 698, 704, 723, 738),
         Vector(739, 767, 788, 813, 816, 842, 847),
         Vector(851, 853, 947, 972, 974, 986, 990))


val list: Vector[Long] = matr.map { case v => v.head }
// val list: List[Long] = List(148, 350, 432, 553, 661, 739, 851)

val lookUpResult = list.search(lookedUp)
val exists = if (lookUpResult.isInstanceOf[Found]) true else {
  val lineIndex = lookUpResult.insertionPoint - 1
  if (lineIndex < 0) false else matr(lineIndex).search(lookedUp).isInstanceOf[Found]
}
