sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Branch(l,r) => 1 + size(l) + size(r)
  case _ => 1
}

def maximum[A](t: Tree[A]): Int = t match {
  case Branch(l,r) => maximum(l) max maximum(r)
  case _ => _
}

def depth[A](t: Tree[A]): Int = t match {
  case Branch(l,r) => 1 + (depth(l) max depth(r))
  case _ => 1
}

def map[A,B](t: Tree[A])(f:A => B): Tree[B] = t match {
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  case Leaf(a) => Leaf(f(a))
}

