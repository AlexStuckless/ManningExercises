package Either

import List._

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match{
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) =>f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
    case Right(a) => Right(a)
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(a => b.map(bx => f(a, bx)))
  }
}

object EitherExamples{
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match{
    case Cons(eHead,eTail) => eHead.flatMap(h => sequence(eTail).map(t => Cons(h, t)))
    case Nil => Right(Nil:List[A])
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Cons(h,t) => f(h).flatMap(h => traverse(t)(f).map(t => Cons(h, t)))
    case Nil => Right(Nil:List[B])
  }
}
