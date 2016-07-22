import org.scalatest._

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
}