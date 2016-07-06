/**
  * Chapter 1 exercises from the Manning functional programming in Scala text
  */
class ChapterTwo {

  //ex 2.1
  def fib(n: Int): Int = {
    if(n < 0){
      -1
    } else {
      def fibRecursion(fibTerm1: Int, fibTerm2: Int, fibIndex: Int): Int = {
        if(fibIndex != n) {
          fibRecursion(fibTerm2, fibTerm1 + fibTerm2, fibIndex + 1)
        } else {
          fibTerm1
        }
      }
      fibRecursion(0,1,0)
    }
  }

  //ex 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val arraySize = as.length
    def isSortedRecursion(arrayIndex: Int): Boolean = {
      if(arrayIndex + 1 >= arraySize) {
        true
      } else if(ordered(as(arrayIndex),as(arrayIndex + 1))) {
        isSortedRecursion(arrayIndex + 1)
      } else {
        false
      }
    }
    isSortedRecursion(0)
  }

  //ex 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a :A => f(a, _ :B)
  }

  //ex 2.4
  //this is like f: A => (B => C)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }

}
