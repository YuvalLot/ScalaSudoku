package Main

import Main.Sudoku.parseBoard
import Main.TechniqueIterators.{groupsWithQuadruple, groupsWithTriple, kiteIterator, rectangleIterator, rectangleIterator4, threeKiteIterator}
import Main.Techniques.{AvoidableRectangle, ColorChain, FinnedXWing, HiddenDouble, HiddenRectangleType1, HiddenRectangleType2, HiddenSingle, HiddenTriple, LockedCandidate, NakedDouble, NakedQuadruple, NakedSingle, NakedTriple, PointingRectangle, PointingTuple, Skyscraper, SueDeCoqType1, SueDeCoqType2, Swordfish, TwoKite, UniqueRectangleType1, UniqueRectangleType2, XWing, XYZWing, YWing}

object SudokuExamples {

  val emptyBoard: String =
    """0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |""".stripMargin

  val beg1: String =
    """0 0 0 0 0 0 4 0 3
      |3 6 0 8 4 0 0 9 0
      |4 8 0 2 5 0 0 0 7
      |6 0 5 0 0 0 0 0 0
      |0 0 0 0 7 2 0 0 9
      |9 4 0 3 0 5 8 7 1
      |2 0 0 0 0 0 7 5 0
      |0 0 0 0 3 7 0 4 0
      |7 5 4 0 0 9 0 3 0
      |""".stripMargin

  def pro1: String =
    """0 0 0 0 0 0 0 0 5
      |5 0 0 0 0 8 0 0 1
      |0 3 0 0 6 0 0 4 0
      |0 0 0 7 0 0 0 0 2
      |3 7 0 5 0 2 0 9 8
      |4 0 0 0 8 6 0 0 0
      |0 2 0 0 3 0 0 5 0
      |8 0 0 1 0 0 0 0 4
      |0 0 0 0 0 0 0 0 0
      |""".stripMargin

  def pro2: String =
    """1 0 0 3 8 0 9 0 0
      |0 0 0 0 0 1 2 0 0
      |7 0 5 0 0 0 0 0 4
      |6 0 0 0 0 2 0 0 0
      |3 0 0 0 0 0 0 0 1
      |0 0 8 7 0 0 0 0 5
      |5 0 0 0 9 0 0 0 8
      |0 0 1 5 0 0 0 3 0
      |0 0 3 0 1 8 0 0 9
      |""".stripMargin

  def XYZWingExample: String =
    """1 7 6 0 0 5 0 2 0
      |3 2 4 0 0 7 0 8 5
      |5 0 0 0 0 2 0 6 0
      |9 6 2 0 5 0 0 3 0
      |0 0 1 2 7 0 5 0 6
      |0 5 0 0 1 0 8 0 2
      |2 4 0 5 0 1 6 7 0
      |0 1 0 9 0 0 2 5 3
      |6 0 5 7 2 0 0 1 0
      |""".stripMargin

  def swordfishExample: String =
    """0 0 4 2 0 0 0 0 3
      |3 2 0 7 0 0 4 8 0
      |8 0 0 0 4 3 9 2 0
      |1 8 2 4 3 7 6 0 0
      |6 4 3 0 0 0 0 0 2
      |0 0 0 6 1 2 3 4 8
      |0 3 6 8 0 0 2 0 4
      |2 1 5 0 0 4 8 0 0
      |4 0 8 0 2 6 5 0 0
      |""".stripMargin

  def uniqueRectangleT1Example: String =
    """0 3 2 6 1 9 0 0 0
      |0 1 9 2 4 7 6 0 3
      |0 0 7 5 3 8 1 2 9
      |9 0 4 8 0 3 5 6 1
      |0 0 3 4 5 1 2 9 0
      |0 5 0 9 0 6 3 0 4
      |3 0 5 7 0 4 9 1 0
      |0 0 0 1 0 5 0 3 0
      |0 0 0 3 0 2 7 0 0
      |""".stripMargin

  def twoStringKiteExample: String =
    """1 0 5 7 0 0 0 0 2
      |0 0 0 6 0 2 5 0 0
      |0 6 0 0 5 1 4 8 0
      |9 3 0 8 2 5 7 0 4
      |0 0 4 0 0 7 8 2 0
      |0 0 7 4 1 6 3 9 5
      |0 0 0 5 7 9 0 4 0
      |0 0 9 2 0 0 0 0 0
      |4 0 0 1 6 3 0 0 0
      |""".stripMargin

  def colorChainExample: String =
    """9 0 1 3 8 6 2 5 0
      |0 0 0 1 0 5 0 9 0
      |0 5 0 0 9 4 0 1 0
      |5 1 0 0 0 9 3 7 8
      |2 0 0 8 0 7 0 6 5
      |0 0 7 5 0 3 0 4 2
      |3 6 8 9 0 1 5 2 0
      |0 0 0 0 5 8 0 3 0
      |0 0 5 0 3 2 0 8 0
      |""".stripMargin

  def uniqueRectangleT2Example: String =
    """0 0 0 3 9 7 4 1 0
      |9 0 1 0 2 4 3 5 0
      |0 3 4 0 1 0 0 0 0
      |0 0 8 4 6 3 1 2 9
      |0 0 3 7 8 9 6 4 5
      |6 4 9 2 5 1 7 8 3
      |0 0 5 9 7 0 0 3 4
      |4 9 0 0 3 0 0 7 1
      |3 0 7 1 4 0 0 0 0
      |""".stripMargin

  def avoidableRectangleExample: String =
    """1 2 3 5 6 4 0 0 7
      |9 8 6 2 3 7 4 1 5
      |4 7 5 8 9 1 3 6 2
      |0 0 8 1 0 5 2 0 6
      |5 6 0 0 0 0 0 0 1
      |2 0 1 5 8 0 7 5 4
      |0 0 0 7 1 6 5 2 0
      |0 1 0 9 5 0 6 4 0
      |6 5 0 0 0 0 1 7 0
      |""".stripMargin

  def hiddenRectangleExample: String =
    """0 0 7 0 0 1 0 0 0
      |0 8 2 0 6 7 0 0 0
      |4 0 1 9 5 0 0 7 0
      |1 0 9 0 0 5 7 0 4
      |0 7 0 0 4 0 5 9 0
      |0 0 4 0 7 9 6 0 0
      |7 4 5 2 8 3 1 6 9
      |0 0 0 5 0 6 2 4 7
      |0 0 6 7 0 4 0 5 0
      |""".stripMargin

  def pointingRectangleExample: String =
    """0 0 0 6 0 9 0 2 0
      |2 3 9 0 0 0 4 0 6
      |0 0 7 4 0 2 5 0 0
      |7 5 8 2 9 3 6 1 4
      |1 2 6 8 4 5 0 0 0
      |0 0 0 0 6 0 2 5 8
      |0 0 1 3 2 0 0 0 5
      |0 0 2 0 0 0 1 6 0
      |0 8 0 9 1 6 0 0 2
      |""".stripMargin

  def finnedXWingExample: String =
    """5 3 4 0 6 0 0 0 0
      |8 0 7 0 5 0 0 0 6
      |6 0 9 7 0 0 3 0 5
      |1 7 3 9 2 8 6 5 4
      |4 5 8 0 7 0 1 9 2
      |2 9 6 5 1 4 0 0 3
      |0 0 1 0 0 5 4 0 7
      |0 0 5 0 0 0 0 0 8
      |0 0 2 0 3 0 5 6 0
      |""".stripMargin

  def hiddenRectangleType1Example: String =
    """1 8 4 0 5 6 0 9 3
      |0 0 0 9 4 8 6 1 5
      |9 5 6 3 0 1 0 0 0
      |0 0 3 0 0 0 0 0 6
      |6 1 0 5 8 7 0 3 0
      |7 0 0 0 6 3 1 0 0
      |0 0 0 6 0 5 9 4 0
      |0 0 0 0 0 2 3 0 8
      |0 0 0 8 3 0 5 0 1
      |""".stripMargin

  def sueDeCoqExample: String =
    """0 3 5 4 9 0 2 0 1
      |9 2 4 1 8 0 0 3 0
      |0 6 1 3 2 0 0 0 0
      |3 4 6 2 1 9 8 7 5
      |2 7 9 8 5 0 1 0 0
      |5 1 8 6 7 0 0 2 0
      |6 5 7 9 4 8 3 1 2
      |4 0 2 5 3 1 0 0 0
      |1 0 3 7 6 2 0 0 4
      |""".stripMargin

  def sueDeCoq2Example: String =
    """7 0 0 9 1 0 0 0 0
      |0 9 3 0 8 0 0 6 1
      |0 4 1 0 0 0 0 7 0
      |0 0 9 0 7 0 0 0 0
      |0 6 5 2 0 0 0 0 0
      |1 7 4 0 0 0 0 2 3
      |0 0 0 1 0 9 0 0 0
      |0 5 0 0 0 0 1 8 0
      |0 1 0 0 0 8 2 0 4
      |""".stripMargin

  def skyscraperExample: String =
    """0 0 0 8 0 5 6 4 0
      |6 4 8 2 9 0 0 0 5
      |3 5 7 6 1 4 0 0 0
      |1 6 2 9 8 0 0 5 0
      |0 8 0 5 6 2 0 0 0
      |0 0 0 0 4 1 2 6 8
      |8 0 0 1 5 0 7 3 0
      |0 0 0 0 2 8 0 9 6
      |0 0 0 4 0 0 0 0 0
      |""".stripMargin

  def pro3: String =
    """3 0 0 1 0 5 0 9 0
      |0 2 4 0 7 8 0 3 0
      |0 5 0 0 0 0 0 0 0
      |0 0 0 0 0 2 6 0 1
      |8 0 0 0 0 0 0 0 7
      |0 0 6 4 0 0 0 0 0
      |0 0 0 0 0 0 0 0 0
      |0 3 0 8 6 1 2 7 0
      |0 1 0 5 0 9 0 0 8
      |""".stripMargin

  def core1: String =
    """8 3 0 0 0 7 0 0 0
      |0 0 7 9 0 0 0 8 0
      |0 0 0 0 3 0 5 0 0
      |5 0 0 6 0 0 1 0 0
      |2 8 0 0 0 0 0 0 6
      |6 0 1 0 0 2 0 0 9
      |0 0 5 0 2 0 0 0 0
      |0 2 0 0 0 4 8 0 0
      |0 0 0 0 0 0 0 6 1
      |""".stripMargin

  /*

    1. algebraic methods in data engineering
    2. database management
    3. data structures and algorithms
    4. the human factor in data harboring
    5. stochastic models in performance testing
    6. game theory and economic behavior
    7. machine learning

   */

  /*

    1. Calc 2
    2. Intro to Probability Theory
    3. Computer Structure
    4. Group Theory
    5. Computational Models
    6. Data Structures
    7. Lin Alg 2

   */

  def main(args: Array[String]): Unit = {

    println(s"groupsWithQuadruple: ${rectangleIterator.length}")

    val sud = Solver.flatten(parseBoard(core1).map(_.toSolver))

    val solved = sud.applyTechnique(Solver.standardTechnique)

    println(solved)

  }

}
