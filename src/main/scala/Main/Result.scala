package Main

import Main.Techniques.Technique

case class Result(output: Option[Sudoku], found: Boolean, move: String) {
  def _1: Option[Sudoku] = output
  def _2: Boolean = found

  def flatMap(f: Sudoku => Result): Result = {
    if (found)
      this
    else output match {
      case Some(value) =>
        f(value)
      case None => this
    }
  }

  def applyTechnique(t: Technique): Result =
    flatMap(t.apply)

  def andAlso(f: Sudoku => Result, join: Boolean = false): Result = {
    output match {
      case Some(sud) =>
        val Result(output2, found2, move2) = f(sud)
        Result(output2, found || found2, if (join) move + move2 else move2)
      case None =>
        this
    }
  }

}

object Result {
  def empty: Result = Result(None, found = false, "")
}
