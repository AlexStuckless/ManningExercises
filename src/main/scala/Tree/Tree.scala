package Tree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/*
Note to self: this is like the most jank ass tree ever. Branches aren't actually nodes (don't have values)
Lowkey disturbing
 */
object Tree{
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l,r) => 1 + size(l) + size(r)
    case _ => 1
  }

  def maximum[A](t: Tree[Int]): Int = t match {
    case Branch(l,r) => maximum(l) max maximum(r)
    case Leaf(a) => a
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l,r) => 1 + (depth(l) max depth(r))
    case _ => 1
  }

  def map[A,B](t: Tree[A])(f:A => B): Tree[B] = t match {
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a) => Leaf(f(a))
  }

  def fold[A,B](t: Tree[A])(f:A => B)(g:(B,B) => B): B = t match {
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(a) => f(a)
  }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x,y) => 1 + x + y)

  def maximumFold(t: Tree[Int]): Int =
    fold(t)(a => a)((x,y) => x max y)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x,y) => 1 + (x max y))

  def mapFold[A,B](t: Tree[A])(f:A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)):Tree[B])((x,y) => Branch(x,y))
}



