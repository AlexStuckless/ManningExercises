package List

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

  /*This one was really hard for me - my underlying understanding of folds is not the greatest. :(
  This was my attempt but it was just like write fold left a different way instead of it actually being a
  fold right- I can understand why - I haven't actually 'reversed' the direction of the fold, just modified the
  function:
  def foldRightFromFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as,z)((b:B, a:A) => f(a,b))
  }

  def foldLeftFromFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a:A, b:B) => f(b,a))
  }

  I needed to look up the answer and this stackoverflow answer was pretty useful:
  http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala
  */

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def concenateLists[A](listOfLists: List[List[A]]): List[A] = {
    foldRight(listOfLists, Nil:List[A])(append(_,_))
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((x:Int, y:List[Int]) => Cons(x+1, y))
  }

  def doublesToStrings(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((d:Double, ls:List[String]) => Cons(d.toString, ls))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a:A, lb:List[B]) => Cons(f(a),lb))

  //this looks jank as hell - can I do it in a nicer way??
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as,Nil:List[A])((a:A, l:List[A]) => {
      if(f(a))
        Cons(a, l)
      else
        l
    })
  }

  def removeOdds(as: List[Int]): List[Int] =
    filter(as)(a => a % 2 == 0)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((a:A, lb:List[B]) => append(f(a), lb))
  }

  //still super ass janky - got to be a better way
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a:A) => {
      if(f(a)) List(a)
      else Nil
    })
  }


  //went full retard - dont know how to do with a fold/map :(
  //can you even ??
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match{
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(h1+h2, addLists(t1,t2))
    case (Cons(h1,t1), Nil) => Cons(h1,t1)
    case (Nil, Cons(h2,t2)) => Cons(h2,t2)
    case (Nil,Nil) => Nil
  }

  //had originally planned cases like above for uneven length lists but didn't have a way to use function f
  //to create the C type
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1, l2) match{
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    case _ => Nil:List[C]
  }

  //sooooo jank O(n^2)
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def checkForSubsequence(interiorSup: List[A], interiorSub: List[A]): Boolean = (interiorSup, interiorSub) match {
      case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => checkForSubsequence(t1,t2)
      case (_, Nil) => true
      case (_,_) => false
    }
    if(checkForSubsequence(sup, sub))
      true
    else if(sup != Nil)
      hasSubsequence(tail(sup), sub)
    else
      false
  }

}
