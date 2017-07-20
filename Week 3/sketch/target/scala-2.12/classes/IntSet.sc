abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

// sets implemented as binary trees here
class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x >= elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  override def toString = "{" + left + elem + right + "}"
}

val treeOne: NonEmpty = new NonEmpty(3, new Empty, new Empty)
val treeTwo = new NonEmpty(5, treeOne, new Empty)
val treeThree = treeOne.incl(2)
val treeFour = treeTwo.incl(8)


object Hello {
  def main(args: Array[String]) = println("Hello world!")
}