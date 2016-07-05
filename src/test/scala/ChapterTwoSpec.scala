import org.scalatest._

class ChapterTwoSpec extends FlatSpec with Matchers {

  "A fibonacci sequence" should "return 0 for the 0th fib number" in {
    val chapter2 = new ChapterTwo()
    chapter2.fib(0) should be (0)
  }

  "A fibonacci sequence" should "return 1 for the 1st fib number" in {
    val chapter2 = new ChapterTwo()
    chapter2.fib(1) should be (1)
  }

  "A fibonacci sequence" should "return 55 for the 10th fib number" in {
    val chapter2 = new ChapterTwo()
    chapter2.fib(10) should be (55)
  }

  "A fibonacci sequence" should "return -1 for the -12th fib number" in {
    val chapter2 = new ChapterTwo()
    chapter2.fib(-12) should be (-1)
  }

  "A isSorted" should "return true when its an empty array" in {
    val chapter2 = new ChapterTwo
    val emptyArray = new Array[Int](0)
    chapter2.isSorted(emptyArray, (x: Int, y:Int) => x > y) should be (true)
  }

  "A isSorted" should "return true when its a 1 element array" in {
    val chapter2 = new ChapterTwo
    val oneElementArray = Array(1)
    chapter2.isSorted(oneElementArray, (x: Int, y:Int) => x > y) should be (true)
  }

  "A isSorted" should "return true when the array is sorted" in {
    val chapter2 = new ChapterTwo
    val sortedArray = Array("Art Garfunkel", "Bob Dylan", "Joan Baez", "Paul Simon")
    chapter2.isSorted(sortedArray, (x: String, y:String) => x.head < y.head) should be (true)
  }

  "A isSorted" should "return false when the array is not sorted" in {
    val chapter2 = new ChapterTwo
    val sortedArray = Array(1, 7, -12, 4)
    chapter2.isSorted(sortedArray, (x: Int, y:Int) => x > y) should be (false)
  }

}
