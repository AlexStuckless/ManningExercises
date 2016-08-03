import org.scalatest._
import EitherExamples._

class EitherSpec extends FlatSpec with Matchers {

  "A mapped Right(_) option" should "transform the value inside a Right" in {
    Right(2).map(x => x.toString) should be(Right("2"))
  }

  "A mapped Left(e) option" should "still be a Left(e)" in {
    Left("exception").map((x:String) => x + " mapped") should be(Left("exception"))
  }

  "A flatMapped Right(_) option" should "transform the value inside a Right" in {
    Right(1).flatMap(x => Right(x*2)) should be(Right(2))
  }

  "A flatMapped Right(_) option that has an invalid transform" should "become a Left" in {
    Right(1).flatMap(x => Left("exception")) should be(Left("exception"))
  }

  "A flatMapped Left(e) option" should "still be a Left(e)" in {
    Left("exception").flatMap((x:String) => Right(x + " mapped")) should be(Left("exception"))
  }

  "Right(_) that has orElse called on it" should "return the same Right(_)" in {
    Right(12324321).orElse(Right(0)) should be(Right(12324321))
  }

  "Left(_) that has orElse called on it" should "return the alternative either" in {
    Left("exception").orElse(Right(23)) should be(Right(23))
  }

  "Two Right(_) that have map2 called on them" should "return Right(f(_,_))" in {
    Right("a").map2(Right(1))((a:String,b:Int) => a.concat(b.toString)) should be(Right("a1"))
  }

  "A Right(_) and a Left(e) that have map2 called on them" should "return Left(e)" in {
    Right("a").map2(Left("exception"))((a:String,b:Int) => a.concat(b.toString)) should be(Left("exception"))
  }

  "Two Left(_) that have map2 called on them" should "return the first Left" in {
    Left("exception1").map2(Left("exception2"))((a:String,b:Int) => a.concat(b.toString)) should be(Left("exception1"))
  }

  "A list of Right(_) that has sequence called on it" should "return Right(List(_))" in {
    sequence(List(Right(1),Right(2), Right(3))) should be(Right(List(1,2,3)))
  }

  "A list with a Left(e) in it that has sequence called on it" should "return Left(e)" in {
    sequence(List(Right(1),Left("exception"), Right(3))) should be(Left("exception"))
  }

  "A list with multiple Left(_) in it that has sequence called on it" should "return the first Left(_)" in {
    sequence(List(Right(1),Left("exception1"),Left("exception2"))) should be(Left("exception1"))
  }


  "A list that has traverse called on it" should "return Right(List(_))" in {
    traverse(List(1,2,3))(x => Right(x.toString)) should be(Right(List("1","2","3")))
  }

  "A list that has traverse called on it that creates a Left(_)" should "return the first Left created" in {
    traverse(List(1,2,3))(x => Left(x)) should be(Left(1))
  }

}