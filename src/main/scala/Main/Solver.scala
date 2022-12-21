package Main

import Techniques._

case class Solver(sud: Option[Sudoku], moves: List[String]) {

  def addResult(result: Result): Solver =
    Solver(
      result.output,
      result.move :: moves
    )

  def applyTechnique(tech: Technique): Solver = sud match {
    case Some(value) =>
      val res = tech.apply(value)
      if (res.found)
        addResult(res).applyTechnique(tech)
      else
        this

    case None => this
  }

  def applyTechniqueOnce(technique: Technique): Solver = sud match {
    case Some(value) =>
      val res = technique.apply(value)
      if (res.found)
        addResult(res)
      else
        this

    case None => this
  }

  def get: (Option[Sudoku], List[String]) = (sud, moves.reverse)

  override def toString: String =
    moves.reverse.mkString("\n") + "\n" + (sud match {
      case Some(value) => value.toString
      case None => "Unsolvable..."
    })

}

object Solver {

  def standardTechnique: Technique =
    OrderedSolver(
      List(NakedSingle, HiddenSingle, NakedDouble, HiddenDouble, PointingTuple, LockedCandidate,
        NakedTriple, HiddenTriple, XWing, YWing, XYZWing, Swordfish, NakedQuadruple,
        HiddenQuadruple, Jellyfish, FinnedXWing,  TwoStringKite, ColorChain,

        SueDeCoqType1, SueDeCoqType2, Skyscraper,

        UniqueRectangleType1, UniqueRectangleType2, UniqueRectangleType3, HiddenRectangleType2,
        HiddenRectangleType1, AvoidableRectangle)
    )

  def solve(sudoku: Sudoku): Solver =
    sudoku.toSolver.applyTechnique(standardTechnique)

  def flatten(option: Option[Solver]): Solver =
    option match {
      case Some(value) => value

      case None => Solver(None, Nil)
    }
}
