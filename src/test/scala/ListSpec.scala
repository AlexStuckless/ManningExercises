import org.scalatest._
import List._

class ListSpec extends FlatSpec with Matchers {

  //exercise 3.1
  "This list" should "return 5 on this matching" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y //this case is the first matching one x=1, y=2
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x should be(3)
  }

  "A empty list" should "return Nil for its tail" in {
    val emptyList = List()
    tail(emptyList) should be(Nil)
  }

  "A 1 element list" should "return Nil for its tail" in {
    val oneElementList = List(1)
    tail(oneElementList) should be(Nil)
  }

  "A multi element list" should "return a list not including the head for its tail" in {
    val multiElementList = List(1,2,3,4)
    tail(multiElementList) should be(List(2,3,4))
  }

  "A empty list" should "return Nil for setHead" in {
    val emptyList = List()
    setHead(emptyList, 5) should be(Nil)
  }

  "A 1 element list" should "return a List of the set element for setHead" in {
    val oneElementList = List(1)
    setHead(oneElementList, -6) should be(List(-6))
  }

  "A multi element list" should "return a list with a different head for setHead" in {
    val multiElementList = List(1,2,3,4)
    setHead(multiElementList, 22) should be(List(22,2,3,4))
  }

  "A list with more elements than dropped elements" should "return a list without the dropped elements" in {
    val greaterList = List(3,5,1,3)
    drop(greaterList, 2) should be(List(1,3))
  }

  "A list with less elements than dropped elements" should "return nil" in {
    val smallerList = List(9,8)
    drop(smallerList, 3) should be(Nil)
  }

  "A list that has no elements dropped" should "return a list with the same elements" in {
    drop(List(1,2), 0) should be(List(1,2))
  }

  "A list with every element dropped" should "return nil" in {
    drop(List(1,2), 2) should be(Nil)
  }

  "A list where the first element doesn't match" should "return a list with the same elements" in {
    dropWhile(List(1,2,3), (x:Int) => x > 5) should be(List(1,2,3))
  }

  "A list where all elements match the predicate" should "return nil" in {
    dropWhile(List(1,2,3), (x:Int) => x < 5) should be(Nil)
  }

  "A list where n starting elements match the predicate (n < size)" should "return the elements after n" in {
    dropWhile(List(1,2,3), (x:Int) => x < 2) should be(List(2,3))
  }

  "A list where an element is matches the predicate after one that doesn't" should "not remove the element" in {
    dropWhile(List(0,1,2,3), (x:Int) => x % 2 == 0) should be(List(1,2,3))
  }

  "A list with multiple elements" should "include all but the last element for init" in {
    init(List(1,2,3)) should be(List(1,2))
  }

  "A list with one element" should "return nil for init" in {
    init(List(1)) should be(Nil)
  }

  "A list with no elements" should "return nil for init" in {
    init(Nil) should be(Nil)
  }
  "A list constructor folded right on a list" should "return the same list" in {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3))
    /*
    The evaluation looks something like:
    Cons(1, Cons(2, Cons(3, Nil))) = List(1,2,3)
     */
  }

  "The length of a list with elements" should "be the number of elements in it" in {
    List.length(List(1,2,3,4)) should be(4)
  }

  "The length of a Nil list" should "be 0" in {
    List.length(Nil) should be(0)
  }

  "The sumLeft of a list with elements" should "be the sum of all its elements" in {
    sumLeft(List(1,2,3)) should be(6)
  }

  "The sumLeft of a list with no elements" should "be 0" in {
    sumLeft(Nil) should be(0)
  }

  "The productLeft of a list with elements" should "be the product of all its elements" in {
    productLeft(List(1.0, 3.5, 2.0)) should be(7)
  }

  "The productLeft of a list that has a 0 element" should "be 0" in {
    productLeft(List(1.0, 0, 2.0)) should be(0)
  }

  "The productLeft of an empty list" should "be 1" in {
    productLeft(Nil) should be(1)
  }

  "The lengthLeft of a list with elements" should "be the number of elements in it" in {
    lengthLeft(List(1,2,3,4)) should be(4)
  }

  "The lengthLeft of a Nil list" should "be 0" in {
    lengthLeft(Nil) should be(0)
  }

  "A list constructor folded right (from a foldLeft) on a list" should "return the same list" in {
    foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3))
  }

  "A list constructor folded left (from a foldRight) on a list" should "return the reversed list" in {
    foldLeftViaFoldRight(List(1,2,3), Nil:List[Int])((list, element) => Cons(element,list)) should be(List(3,2,1))
  }

  "A list that has another list appended to it" should "have its elements appear first and the other's second" in {
    append(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

  "A list of lists being concenated" should "have the lists merged in order into one big list" in {
    concenateLists(List(List(1,2,3),List(4,5,6),List(7,8,9))) should be(List(1,2,3,4,5,6,7,8,9))
  }

  "A list that has addOne called on it" should "return a new list where each element has been incremented" in {
    addOne(List(1,2,3)) should be(List(2,3,4))
  }

}
