case class StackItem(item: Int, thenMax: Int)
case class MaxStack(items: List[StackItem])

def push(s: MaxStack, value: Int): MaxStack = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , thenMax) =>
    val newMax = scala.math.max(item, thenMax)
    MaxStack(StackItem(value, newMax) :: s.items)
  }.getOrElse {
    MaxStack(List(StackItem(value, 0)))
  }
}

def pop(s: MaxStack): (Option[Int], MaxStack) = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , _) =>
    val restStack = MaxStack(s.items.tail)
    (Some(item), MaxStack(s.items.tail))
  }.getOrElse {
    (None, s)
  }
}

def max(s: MaxStack): Option[Int] = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , thenMax) =>
    scala.math.max(item, thenMax)
  }
}
