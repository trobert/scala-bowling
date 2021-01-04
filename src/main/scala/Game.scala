class Game(maxTurns: Int = 10) {
  private var _score: Int = 0
  private var currentTurn = 1

  // If we are in the middle of a frame, this will be defined with the score of the first roll
  private var firstPinsInFrame: Option[Int] = None

  // How many times the next two rolls will contribute to the score
  // (2 for previous spare or strike, 3 for previous double strike)
  private var nextPinMultipliers = (1, 1)


  def pins(pins: Int): Unit = {
    require(pins > 0, "pins must be positive")
    val (currentM, nextM) = nextPinMultipliers

    // adjust the multiplier to no overcount the end game extra rolls
    // (for maxTurn + 1 multiplier is off by 1 and for maxTurn + 2 it is off by 2)
    val m = currentM - (currentTurn - maxTurns).max(0)

    require(m > 0, "unexpected roll, game is already over")

    _score += m * pins

    firstPinsInFrame match {
      case None if pins == 10                      =>
        // strike
        nextPinMultipliers = (nextM + 1, 2)
        currentTurn += 1
      case Some(prevPins) if prevPins + pins == 10 =>
        // spare
        nextPinMultipliers = (nextM + 1, 1)
        firstPinsInFrame = None
        currentTurn += 1
      case None                                    =>
        // first roll in frame
        require(pins < 10, "pins must be below 10")
        nextPinMultipliers = (nextM, 1)
        firstPinsInFrame = Some(pins)
      case Some(prevPins)                          =>
        // second roll in frame
        require(prevPins + pins < 10, "pins sum in a frame must be below 10")
        nextPinMultipliers = (nextM, 1)
        firstPinsInFrame = None
        currentTurn += 1
    }
  }

  def score: Int = _score

}
