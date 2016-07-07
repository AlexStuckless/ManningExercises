sealed trait FpList[+A]
case object Nil extends FpList[Nothing]
case class Cons[+A](head: A, tail: FpList[A]) extends FpList[A]
object FpList {
  def sum(ints: FpList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FpList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): FpList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: FpList[A]): FpList[A] = list match{
    case Cons(_, t) => t
    case _ => Nil
  }

  def setHead[A](list: FpList[A], newHead: A): FpList[A] = list match{
    case Cons(_, t) => Cons(newHead, t)
    case _ => Nil
  }

}
