import Option.OptionExamples
import org.scalatest._
import OptionExamples._
import List._

class OptionSpec extends FlatSpec with Matchers {

  "A mapped Option.Some(_) option" should "transform the value inside a Option.Some" in {
    Option.Some(6).map(x => x.toString) should be(Option.Some("6"))
  }

  "A mapped None option" should "still be a None" in {
    Option.None.map(x => x.toString) should be(Option.None)
  }

  "A flatMapped Option.Some(_) option" should "transform the value inside a Option.Some" in {
    Option.Some(1).flatMap(x => Option.Some(x*2)) should be(Option.Some(2))
  }

  "A flatMapped None option" should "still be a None" in {
    Option.None.flatMap(x => Option.Some(x.toString)) should be(Option.None)
  }

  "Option.Some(_) that has getOrElse called on it" should "return _" in {
    Option.Some(2).getOrElse(1) should be(2)
  }

  "None that has getOrElse called on it" should "return the default" in {
    Option.None.getOrElse(3) should be(3)
  }

  "Option.Some that has orElse called on it" should "return the same option" in {
    Option.Some(12324321).orElse(Option.Some(0)) should be(Option.Some(12324321))
  }

  "None that has orElse called on it" should "return the alternative option" in {
    Option.None.orElse(Option.Some(23)) should be(Option.Some(23))
  }

  "Option.Some(_) that matches the filter predicate" should "return the same Option.Some(_)" in {
    Option.Some(4).filter(x => x % 2 == 0) should be(Option.Some(4))
  }

  "Option.Some(_) that fails the filter" should "return None" in {
    Option.Some(3).filter(x => x % 2 == 0) should be(Option.None)
  }

  "None that is filtered" should "still return none" in {
    Option.None.filter(x => true) should be(Option.None)
  }

  "The variance of a sequence of elements" should "return Option.Some(variance) when the seq isn't empty" in {
    variance(Seq(1,2,3,4)) should be(Option.Some(1.25)) //according to variance calculator online
  }

  "The variance of a sequence with no elements" should "return None" in {
    variance(Seq()) should be(Option.None) //according to variance calculator online
  }

  "Two Option.Some(_) that have map2 called on them" should "return Option.Some(f(_,_))" in {
    map2(Option.Some("a"), Option.Some(1))((a:String, b) => a.concat(b.toString)) should be(Option.Some("a1"))
  }

  "A Option.Some(_) and a None that have map2 called on them" should "return None" in {
    map2(Option.Some("a"), Option.None:Option.Option[Int])((a:String, b) => a.concat(b.toString)) should be(Option.None)
  }

  "A None and a Option.Some(_) that have map2 called on them" should "return None" in {
    map2(Option.None:Option.Option[String], Option.Some(1))((a:String, b) => a.concat(b.toString)) should be(Option.None)
  }

  "Two None that have map2 called on them" should "return None" in {
    map2(Option.None:Option.Option[String], Option.None:Option.Option[Int])((a:String, b) => a.concat(b.toString)) should be(Option.None)
  }

  "A list of Option.Some(_) that has sequence called on it" should "return Option.Some|(List(_))" in {
    sequence(List(Option.Some(1),Option.Some(2), Option.Some(3))) should be(Option.Some(List(1,2,3)))
  }

  "A list with a None in it that has sequence called on it" should "return None" in {
    sequence(List(Option.Some(1),Option.None, Option.Some(3))) should be(Option.None)
  }

  "A list of Option.Some(_) that has sequenceTraverse called on it" should "return Option.Some|(List(_))" in {
    sequenceTraverse(List(Option.Some(1),Option.Some(2), Option.Some(3))) should be(Option.Some(List(1,2,3)))
  }

  "A list with a None in it that has sequenceTraverse called on it" should "return None" in {
    sequenceTraverse(List(Option.Some(1),Option.None, Option.Some(3))) should be(Option.None)
  }

}