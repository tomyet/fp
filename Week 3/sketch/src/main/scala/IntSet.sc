abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

// sets implemented as binary trees here
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: IntSet) = other
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

  def union(other: IntSet) =
    ((left union right) union other) incl elem
  // this recursion will terminate as left and right will eventually evaluate to the empty set
}

val treeOne: NonEmpty = new NonEmpty(3, Empty, Empty)
val treeTwo = new NonEmpty(5, treeOne, Empty)
val treeThree = treeOne.incl(2)
val treeFour = treeTwo.incl(8)

