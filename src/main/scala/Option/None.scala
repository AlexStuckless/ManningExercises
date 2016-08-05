package Option

import List._

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  // Option Some(_) / None --> Some(1).map(x => x+1) == Some(2) None.map(x =>x +1) == None
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if(f(a)) Some(a) else None)
}

object OptionExamples{
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val xsMean = mean(xs)
    val varianceSeq: Option[Seq[Double]] = xsMean.map(m => xs.map(x => math.pow(x - m, 2)))
    varianceSeq.flatMap(mean)
  }

  //So pretty easy to see how you get map 3, etc. But what about mapX? Is there a way to just make
  //a map for a generic number of arguments????
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap( x => b.map( y => f(x,y)))
  }

  //This was janky multiple iterations in my notebook to get to this point
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case(Cons(h,t)) => h.flatMap( x => sequence(t).map(y => Cons(x,y)))
    case(Nil) => Some(Nil:List[A])
  }

  //so pretty much identical to the above logic but just sneak in the transform
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case(Cons(h,t)) => f(h).flatMap( x => traverse(t)(f).map( y => Cons(x,y)))
    case(Nil) => Some(Nil:List[B])
  }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
