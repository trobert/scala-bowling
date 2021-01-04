import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyFlatSpec with Matchers {
  "Game" should "sum pins" in {
    val game = new Game(3)
    Seq(1, 2, 3, 4, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (3 + 4) + (5 + 4))
  }

  it should "handle spare" in {
    val game = new Game(3)
    Seq(1, 2, 3, 7, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (3 + 7 + 5) + (5 + 4))
  }

  it should "handle strike" in {
    val game = new Game(3)
    Seq(1, 2, 10, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 5 + 4) + (5 + 4))
  }

  it should "handle spare + strike" in {
    val game = new Game(4)
    Seq(1, 2, 3, 7, 10, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (3 + 7 + 10) + (10 + 5 + 4) + (5 + 4))
  }

  it should "handle double strike" in {
    val game = new Game(4)
    Seq(1, 2, 10, 10, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 10 + 5) + (10 + 5 + 4) + (5 + 4))
  }

  it should "handle triple strike" in {
    val game = new Game(5)
    Seq(1, 2, 10, 10, 10, 5, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 10 + 10) + (10 + 10 + 5) + (10 + 5 + 4) + (5 + 4))
  }

  it should "handle final spare" in {
    val game = new Game(2)
    Seq(1, 2, 3, 7, 5).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (3 + 7 + 5))
  }

  it should "handle final strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 3, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 3 + 4))
  }

  it should "handle final double strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 10, 4).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 10 + 4))
  }

  it should "handle penultimate double strike" in {
    val game = new Game(3)
    Seq(1, 2, 10, 10, 4, 2).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 10 + 4) + (10 + 4 + 2))
  }

  it should "handle final triple strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 10, 10).foreach(game.pins)
    game.score shouldBe ((1 + 2) + (10 + 10 + 10))
  }

  it should "throw when given too many pins" in {
    val game = new Game(3)
    an[IllegalArgumentException] shouldBe thrownBy {
      Seq(1, 2, 3, 4, 5, 4, 2).foreach(game.pins)
    }
  }

  it should "throw when given too many pins after final spare" in {
    val game = new Game(2)
    Seq(1, 2, 3, 7, 5).foreach(game.pins)
    an[IllegalArgumentException] shouldBe thrownBy {
      game.pins(4)
    }
  }

  it should "throw when given an extra pin after final spare (with ending strike)" in {
    val game = new Game(2)
    Seq(1, 2, 3, 7, 10).foreach(game.pins)
    an[IllegalArgumentException] shouldBe thrownBy {
      game.pins(1)
    }
  }

  it should "throw when given an extra pin after final strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 5, 4).foreach(game.pins)
    an[IllegalArgumentException] shouldBe thrownBy {
      game.pins(2)
    }
  }

  it should "throw when given an extra pin after final double strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 10, 4).foreach(game.pins)
    an[IllegalArgumentException] shouldBe thrownBy {
      game.pins(2)
    }
  }

  it should "throw when given an extra pin after final triple strike" in {
    val game = new Game(2)
    Seq(1, 2, 10, 10, 10).foreach(game.pins)
    an[IllegalArgumentException] shouldBe thrownBy {
      game.pins(1)
    }
  }
}
