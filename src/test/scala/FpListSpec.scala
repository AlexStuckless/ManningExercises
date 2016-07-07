import org.scalatest._
import FpList._

class FpListSpec extends FlatSpec with Matchers {

  //exercise 3.1
  "This list" should "return 5 on this matching" in {
    val x = FpList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y //this case is the first matching one x=1, y=2
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be(3)
  }

  "A empty list" should "return Nil for its tail" in {
    val emptyList = FpList()
    tail(emptyList) should be(Nil)
  }

  "A 1 element list" should "return Nil for its tail" in {
    val oneElementList = FpList(1)
    tail(oneElementList) should be(Nil)
  }

  "A multi element list" should "return a list not including the head for its tail" in {
    val multiElementList = FpList(1,2,3,4)
    tail(multiElementList) should be(FpList(2,3,4))
  }

  "A empty list" should "return Nil for setHead" in {
    val emptyList = FpList()
    setHead(emptyList, 5) should be(Nil)
  }

  "A 1 element list" should "return a List of the set element for setHead" in {
    val oneElementList = FpList(1)
    setHead(oneElementList, -6) should be(FpList(-6))
  }

  "A multi element list" should "return a list with a different head for setHead" in {
    val multiElementList = FpList(1,2,3,4)
    setHead(multiElementList, 22) should be(FpList(22,2,3,4))
  }


}
