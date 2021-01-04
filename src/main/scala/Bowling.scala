object Bowling {

  type Frame = Seq[Int]

  private val emptyFrame: Frame = Seq.empty[Int]

  def score(rolls: Seq[Frame]): Int =
      (rolls :+ emptyFrame :+ emptyFrame).sliding(3).map {
        case Seq(Seq(x, y), nextFrame, _) if x + y == 10 => 10 + nextFrame.head
        case Seq(Seq(10), nextFrame, nextFrame2)         => 10 + (nextFrame ++ nextFrame2).take(2).sum
        case Seq(frame, _, _) => frame.sum
      }.sum

  /* An initial version which I did incrementally in a TDD fashion */
  def scoreOld(rolls: Seq[Frame]): Int =
    rolls.map(_.sum).sum +
      (rolls :+ emptyFrame).sliding(3).collect {
        case Seq(Seq(x, y), nextFrame, _) if x + y == 10 => nextFrame.head
        case Seq(Seq(10), nextFrame, nextFrame2)         => (nextFrame ++ nextFrame2).take(2).sum
      }.sum

  
}