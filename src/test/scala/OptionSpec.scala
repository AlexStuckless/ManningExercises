import org.scalatest._
import OptionExamples._

class OptionSpec extends FlatSpec with Matchers {

  "A mapped Some(_) option" should "transform the value inside a Some" in {
    Some(6).map(x => x.toString) should be(Some("6"))
  }

  "A mapped None option" should "still be a None" in {
    None.map(x => x.toString) should be(None)
  }

  "A flatMapped Some(_) option" should "transform the value inside a Some" in {
    Some(1).flatMap(x => Some(x*2)) should be(Some(2))
  }

  "A flatMapped None option" should "still be a None" in {
    None.flatMap(x => Some(x.toString)) should be(None)
  }

  "Some(_) that has getOrElse called on it" should "return _" in {
    Some(2).getOrElse(1) should be(2)
  }

  "None that has getOrElse called on it" should "return the default" in {
    None.getOrElse(3) should be(3)
  }

  "Some that has orElse called on it" should "return the same option" in {
    Some(12324321).orElse(Some(0)) should be(Some(12324321))
  }

  "None that has orElse called on it" should "return the alternative option" in {
    None.orElse(Some(23)) should be(Some(23))
  }

  "Some(_) that matches the filter predicate" should "return the same Some(_)" in {
    Some(4).filter(x => x % 2 == 0) should be(Some(4))
  }

  "Some(_) that fails the filter" should "return None" in {
    Some(3).filter(x => x % 2 == 0) should be(None)
  }

  "None that is filtered" should "still return none" in {
    None.filter(x => true) should be(None)
  }

  "The variance of a sequence of elements" should "return Some(variance) when the seq isn't empty" in {
    variance(Seq(1,2,3,4)) should be(Some(1.25)) //according to variance calculator online
  }

  "The variance of a sequence with no elements" should "return None" in {
    variance(Seq()) should be(None) //according to variance calculator online
  }

  "Two Some(_) that have map2 called on them" should "return Some(f(_,_))" in {
    map2(Some("a"), Some(1))((a:String,b) => a.concat(b.toString)) should be(Some("a1"))
  }

  "A Some(_) and a None that have map2 called on them" should "return None" in {
    map2(Some("a"), None:Option[Int])((a:String,b) => a.concat(b.toString)) should be(None)
  }

  "A None and a Some(_) that have map2 called on them" should "return None" in {
    map2(None:Option[String], Some(1))((a:String,b) => a.concat(b.toString)) should be(None)
  }

  "Two None that have map2 called on them" should "return None" in {
    map2(None:Option[String], None:Option[Int])((a:String,b) => a.concat(b.toString)) should be(None)
  }

  "A list of Some(_) that has sequence called on it" should "return Some|(List(_))" in {
    sequence(List(Some(1),Some(2), Some(3))) should be(Some(List(1,2,3)))
  }

  "A list with a None in it that has sequence called on it" should "return None" in {
    sequence(List(Some(1),None, Some(3))) should be(None)
  }

  "A list of Some(_) that has sequenceTraverse called on it" should "return Some|(List(_))" in {
    sequenceTraverse(List(Some(1),Some(2), Some(3))) should be(Some(List(1,2,3)))
  }

  "A list with a None in it that has sequenceTraverse called on it" should "return None" in {
    sequenceTraverse(List(Some(1),None, Some(3))) should be(None)
  }

}