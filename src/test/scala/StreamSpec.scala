import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "A non-empty stream that has toList called on it" should "return a list of its evaluated elements" in {

  }

  "An empty stream that has toList called on it" should "return a Nil list" in {

  }

  "A stream that has take(n) called on it where n < size(stream)" should "return a stream of the first n elements" in {

  }

  "A stream that has take(n) called on it where n >= size(stream)" should "return a stream of the all elements" in {

  }

  "A stream that has drop(n) called on it where n < size(stream)" should "return a stream with all elements after the first n" in {

  }

  "A stream that has drop(n) called on it where n > size(stream)" should "return an empty stream" in {

  }

  "A stream that has takeWhile called on it" should "return a new stream of the first n elements where the condition holds" in {

  }

  "A stream that has takeWhile called on it and has a matching after a failure" should "not include the matching element" in {

  }

  "A stream that has forAll called on it" should "return true if all elements match the predicate" in {

  }

  "A stream that has forAll called on it" should "return false if at least 1 element doesn't match" in {

  }

  "An empty stream that has forAll called on it" should "return true" in {

  }

  "A stream that has takeWhileFoldRight called on it" should "return a new stream of the first n elements where the condition holds" in {

  }

  "A stream that has takeWhileFoldRight called on it and has a matching after a failure" should "not include the matching element" in {

  }

  "A stream that has headOptionFoldRight called on it" should "return Some(h())" in {

  }

  "An empty stream that has headOptionFoldRight called on it" should "return None" in {

  }

  "A stream that has map called on it" should "return a transformed stream without being evaluated" in {

  }

  "A stream that has filter called on it" should "return a stream with any failing elements removed" in {

  }

  "A stream that has append called on it" should "return a stream with the new element added to the end" in {

  }

  "A stream that has flatmap called on it" should "return a transformed and flattened stream" in {

  }


}
