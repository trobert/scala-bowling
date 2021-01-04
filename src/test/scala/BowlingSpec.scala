import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BowlingSpec extends AnyFlatSpec with Matchers {

  "Bowling Scoring" should "sum all pins" in {
    Bowling.score(Seq(Seq(1, 2), Seq(3, 4), Seq(2, 2))) shouldBe 14
  }

  it should "handle spare" in {
    Bowling.score(Seq(Seq(7, 3), Seq(2, 6))) shouldBe 20
  }

  it should "handle final spare" in {
    Bowling.score(Seq(Seq(7, 3), Seq(2, 8, 4))) shouldBe 26
  }

  it should "handle strike" in {
    Bowling.score(Seq(Seq(10), Seq(2, 6))) shouldBe 26
  }

  it should "handle final strike" in {
    Bowling.score(Seq(Seq(6,2), Seq(10, 6, 2))) shouldBe 26
  }

  it should "handle spare+strike" in {
    Bowling.score(Seq(Seq(6,4), Seq(10), Seq(2,2))) shouldBe 38
  }

  it should "handle double strike" in {
    Bowling.score(Seq(Seq(10), Seq(10), Seq(2,2))) shouldBe 40 // 22 + 14 + 4
  }

  it should "handle triple strike" in {
    Bowling.score(Seq(Seq(10), Seq(10), Seq(10), Seq(4,4))) shouldBe 80 // 30 + 24 + 18 + 8
  }

}