package Stream

import Option._

case object Empty extends Stream[Nothing]
case class cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case cons(h, t:Stream[A]) => h() :: t.toList
    case Empty => Nil
  }

}

object Stream {
  def Cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else Cons(as.head, apply(as.tail: _*))
}

