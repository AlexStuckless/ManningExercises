case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

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