import org.scalatest._
import List._

class ListSpec extends FlatSpec with Matchers {

  //exercise 3.1
  "This list" should "return 5 on this matching" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y //this case is the first matching one x=1, y=2
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x should be(3)
  }

  "A empty list" should "return Nil for its tail" in {
    val emptyList = List()
    List.tail(emptyList) should be(Nil)
  }

  "A 1 element list" should "return Nil for its tail" in {
    val oneElementList = List(1)
    List.tail(oneElementList) should be(Nil)
  }

  "A multi element list" should "return a list not including the head for its tail" in {
    val multiElementList = List(1,2,3,4)
    List.tail(multiElementList) should be(List(2,3,4))
  }

  "A empty list" should "return Nil for setHead" in {
    val emptyList = List()
    List.setHead(emptyList, 5) should be(Nil)
  }

  "A 1 element list" should "return a List of the set element for setHead" in {
    val oneElementList = List(1)
    List.setHead(oneElementList, -6) should be(List(-6))
  }

  "A multi element list" should "return a list with a different head for setHead" in {
    val multiElementList = List(1,2,3,4)
    List.setHead(multiElementList, 22) should be(List(22,2,3,4))
  }

  "A list with more elements than dropped elements" should "return a list without the dropped elements" in {
    val greaterList = List(3,5,1,3)
    List.drop(greaterList, 2) should be(List(1,3))
  }

  "A list with less elements than dropped elements" should "return nil" in {
    val smallerList = List(9,8)
    List.drop(smallerList, 3) should be(Nil)
  }

  "A list that has no elements dropped" should "return a list with the same elements" in {
    List.drop(List(1,2), 0) should be(List(1,2))
  }

  "A list with every element dropped" should "return nil" in {
    List.drop(List(1,2), 2) should be(Nil)
  }

  "A list where the first element doesn't match" should "return a list with the same elements" in {
    List.dropWhile(List(1,2,3), (x:Int) => x > 5) should be(List(1,2,3))
  }

  "A list where all elements match the predicate" should "return nil" in {
    List.dropWhile(List(1,2,3), (x:Int) => x < 5) should be(Nil)
  }

  "A list where n starting elements match the predicate (n < size)" should "return the elements after n" in {
    List.dropWhile(List(1,2,3), (x:Int) => x < 2) should be(List(2,3))
  }

  "A list where an element is matches the predicate after one that doesn't" should "not remove the element" in {
    List.dropWhile(List(0,1,2,3), (x:Int) => x % 2 == 0) should be(List(1,2,3))
  }

  "A list with multiple elements" should "include all but the last element for init" in {
    List.init(List(1,2,3)) should be(List(1,2))
  }

  "A list with one element" should "return nil for init" in {
    List.init(List(1)) should be(Nil)
  }

  "A list with no elements" should "return nil for init" in {
    List.init(Nil) should be(Nil)
  }
  "A list constructor folded right on a list" should "return the same list" in {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3))
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
    List.sumLeft(List(1,2,3)) should be(6)
  }

  "The sumLeft of a list with no elements" should "be 0" in {
    List.sumLeft(Nil) should be(0)
  }

  "The productLeft of a list with elements" should "be the product of all its elements" in {
    List.productLeft(List(1.0, 3.5, 2.0)) should be(7)
  }

  "The productLeft of a list that has a 0 element" should "be 0" in {
    List.productLeft(List(1.0, 0, 2.0)) should be(0)
  }

  "The productLeft of an empty list" should "be 1" in {
    List.productLeft(Nil) should be(1)
  }

  "The lengthLeft of a list with elements" should "be the number of elements in it" in {
    List.lengthLeft(List(1,2,3,4)) should be(4)
  }

  "The lengthLeft of a Nil list" should "be 0" in {
    List.lengthLeft(Nil) should be(0)
  }

  "A list constructor folded right (from a foldLeft) on a list" should "return the same list" in {
    List.foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be(List(1,2,3))
  }

  "A list constructor folded left (from a foldRight) on a list" should "return the reversed list" in {
    List.foldLeftViaFoldRight(List(1,2,3), Nil:List[Int])((list, element) => Cons(element,list)) should be(List(3,2,1))
  }

  "A list that has another list appended to it" should "have its elements appear first and the other's second" in {
    List.append(List(1,2), List(3,4)) should be(List(1,2,3,4))
  }

  "A list of lists being concenated" should "have the lists merged in order into one big list" in {
    List.concenateLists(List(List(1,2,3),List(4,5,6),List(7,8,9))) should be(List(1,2,3,4,5,6,7,8,9))
  }

  "A list that has addOne called on it" should "return a new list where each element has been incremented" in {
    List.addOne(List(1,2,3)) should be(List(2,3,4))
  }

  "A list of doubles" should "be convertable to a list of strings" in {
    List.doublesToStrings(List(1.2, 3.4, 0.567)) should be(List("1.2", "3.4", "0.567"))
  }

  "A map on a list" should "transform each element into a new list" in {
    List.map(List(1,2,3))(x => x*2) should be(List(2,4,6))
  }

  "A filtered list" should "return only the elements that match the predicate" in {
    List.filter(List(1,2,3))(x => x % 2 == 1) should be(List(1,3))
  }

  "A list that has removeOdds called on it" should "return a list with all the odds removed" in {
    List.removeOdds(List(1,2,3)) should be(List(2))
  }

  "A list that has flat map called on it" should "return a list with elements transformed into lists and flattened" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be(List(1,1,2,2,3,3))
  }

  "A filtered(2) list" should "return only the elements that match the predicate" in {
    List.filter2(List(1,2,3))(x => x % 2 == 1) should be(List(1,3))
  }

  "Two lists added together" should "return one list with the elements added together" in {
    List.addLists(List(1,2,3),List(4,5,6)) should be(List(5,7,9))
  }

  "Two lists added together where the first is longer" should "return one list with the elements added together" in {
    List.addLists(List(1,2,3),List(4,5)) should be(List(5,7,3))
  }

  "Two lists added together where the second is longer" should "return one list with the elements added together" in {
    List.addLists(List(1,2),List(4,5,6)) should be(List(5,7,6))
  }

  "Two lists zipped together" should "return one list with the elements having a function applied to them" in {
    List.zipWith(List(1,2,3),List(4,5,6))((x,y) => x*y) should be(List(4,10,18))
  }

  "A list with a valid subsequence" should "return true" in {
    List.hasSubsequence(List(1,2,3,4), List(3,4)) should be(true)
  }

  "A list without a valid subsequence" should "return false" in {
    List.hasSubsequence(List(1,2,3,4), List(1,3)) should be(false)
  }

}
