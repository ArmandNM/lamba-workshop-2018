class Node(val n: Int, var l: Option[Node] = None, var r: Option[Node] = None) {
  override def toString: String = s" # ${n} # "
}

val nodeOne = new Node(1)
val nodeTwo = new Node(2)
val nodeThree = new Node(3)
val nodeFour = new Node(4)
val nodeFive = new Node(6)
val nodeSix = new Node(7)

val nodeSeven = new Node(8)
val nodeEight = new Node(9)

nodeOne.l = Some(nodeTwo)
nodeOne.r = Some(nodeThree)

nodeTwo.l = Some(nodeFour)

nodeThree.l = Some(nodeFive)
nodeThree.r = Some(nodeSix)

nodeFour.l = Some(nodeSeven)
nodeTwo.r = Some(nodeEight)

def isBalanced(root: Option[Node]): (Int, Boolean) = {
  if (root.isEmpty) {
    (0, true)
  } else {
    val (heightLeft, isBalancedLeft) = isBalanced(root.get.l)
    val (heightRight, isBalancedRight) = isBalanced(root.get.r)

    val crtHeight = 1 + math.max(heightLeft, heightRight)

    if (math.abs(heightLeft - heightRight) <= 1) {
      (crtHeight, isBalancedLeft && isBalancedRight)
    } else {
      (crtHeight, false)
    }
  }
}

isBalanced(Some(nodeOne))
