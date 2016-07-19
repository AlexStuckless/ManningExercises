sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match{
    case Cons(_, t) => t
    case _ => Nil
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match{
    case Cons(_, t) => Cons(newHead, t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n < 1) l
    else {
      l match {
        case Cons(_, t) => drop(t, n-1)
        case _ => Nil
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Cons(h, t) if f(h) => dropWhile(t,f)
    case otherList => otherList //originally had _ => _ caused compile error "unbound placeholder parameter"
  }

  /*runs in O(n) because underlying list implementation doesn't have ref to last element
  need to iterate thru all of them to find which one is the last one
  */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
    case _ => Nil
  }

  //next 3 are textbook code
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  /*can't immediately halt at first 0 element - needs to traverse thru entire list first
  like a flower bud blooming, go all the way in then start evaluating outwards
  */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, runningTotal) => runningTotal + 1)
  }


  /*Feeling kind of janky about this one - just fiddled to make it match the function definition
  don't feel like I have any deep understanding of how I created it
  */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match{
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case _ => z
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l,0)(_+_)

  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_*_)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((runningTotal, _) => runningTotal + 1)


}
