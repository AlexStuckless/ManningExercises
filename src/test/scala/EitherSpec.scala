import Either.EitherExamples
import org.scalatest._
import EitherExamples._
import List._

class EitherSpec extends FlatSpec with Matchers {

  "A mapped Right(_) option" should "transform the value inside a Right" in {
    Either.Right(2).map(x => x.toString) should be(Either.Right("2"))
  }

  "A mapped Left(e) option" should "still be a Left(e)" in {
    Either.Left("exception").map((x:String) => x + " mapped") should be(Either.Left("exception"))
  }

  "A flatMapped Right(_) option" should "transform the value inside a Right" in {
    Either.Right(1).flatMap(x => Either.Right(x*2)) should be(Either.Right(2))
  }

  "A flatMapped Right(_) option that has an invalid transform" should "become a Left" in {
    Either.Right(1).flatMap(x => Either.Left("exception")) should be(Either.Left("exception"))
  }

  "A flatMapped Left(e) option" should "still be a Left(e)" in {
    Either.Left("exception").flatMap((x:String) => Either.Right(x + " mapped")) should be(Either.Left("exception"))
  }

  "Right(_) that has orElse called on it" should "return the same Right(_)" in {
    Either.Right(12324321).orElse(Either.Right(0)) should be(Either.Right(12324321))
  }

  "Left(_) that has orElse called on it" should "return the alternative either" in {
    Either.Left("exception").orElse(Either.Right(23)) should be(Either.Right(23))
  }

  "Two Right(_) that have map2 called on them" should "return Right(f(_,_))" in {
    Either.Right("a").map2(Either.Right(1))((a:String, b:Int) => a.concat(b.toString)) should be(Either.Right("a1"))
  }

  "A Right(_) and a Left(e) that have map2 called on them" should "return Left(e)" in {
    Either.Right("a").map2(Either.Left("exception"))((a:String, b:Int) => a.concat(b.toString)) should be(Either.Left("exception"))
  }

  "Two Left(_) that have map2 called on them" should "return the first Left" in {
    Either.Left("exception1").map2(Either.Left("exception2"))((a:String, b:Int) => a.concat(b.toString)) should be(Either.Left("exception1"))
  }

  "A list of Right(_) that has sequence called on it" should "return Right(List(_))" in {
    sequence(List(Either.Right(1),Either.Right(2), Either.Right(3))) should be(Either.Right(List(1,2,3)))
  }

  "A list with a Left(e) in it that has sequence called on it" should "return Left(e)" in {
    sequence(List(Either.Right(1),Either.Left("exception"), Either.Right(3))) should be(Either.Left("exception"))
  }

  "A list with multiple Left(_) in it that has sequence called on it" should "return the first Left(_)" in {
    sequence(List(Either.Right(1),Either.Left("exception1"),Either.Left("exception2"))) should be(Either.Left("exception1"))
  }


  "A list that has traverse called on it" should "return Right(List(_))" in {
    traverse(List(1,2,3))(x => Either.Right(x.toString)) should be(Either.Right(List("1","2","3")))
  }

  "A list that has traverse called on it that creates a Left(_)" should "return the first Left created" in {
    traverse(List(1,2,3))(x => Either.Left(x)) should be(Either.Left(1))
  }

}