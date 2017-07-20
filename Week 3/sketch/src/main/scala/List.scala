/**
  * Created by tomyet on 7/18/17.
  */
trait List[T] { //recall traits' definitions look similar to abstract classes
  def isEmpty: Boolean
  def head: T // 'T' here is a type parameter
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}


def nth(n: Int, lst: List[T]): T = {
  if (n == 0) list.head
  else if (lst.isEmpty) throw new IndexOutOfBoundsException
  else nth(n - 1, lst.tail)
}