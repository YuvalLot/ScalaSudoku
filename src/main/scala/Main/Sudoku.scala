package Main


import Main.Sudoku.{allLocs, boxIterator, colIterator, neighIterator, rowIterator}
import Main.SudokuExamples._
import Main._Cell._

import javax.sound.sampled.SourceDataLine
import scala.sys.BooleanProp

case class Sudoku(board: List[List[Cell]]) {

  def toList: List[Cell] =
    board.foldLeft[List[Cell]](List())((x, y) => x ++ y)

  def maxPad: Int =
    (for (c <- toList) yield c match {
      case Empty(options) => 1 + 2 * options.length
      case Full(num, _) => 1
    }).max

  type Loc = (Int, Int)

  // assert(board.length == size && board.forall(_.length == size))
  // assert(board.forall(row => row.forall(_.getSize == size)))

  def affect(loc: Loc, f: Cell => Cell): Option[Sudoku] = {
    val updated = Sudoku(board.updated(loc._1,
      board(loc._1).updated(loc._2, f(board(loc._1)(loc._2)))
    ))
    if (updated.get(loc).stuck) None else Some(updated)
  }

  def updateIterator(iterator: List[Loc])(value: Int): Option[Sudoku] =
    if (iterator.nonEmpty)
      affect(iterator.head, _.without(value))
        .flatMap(_.updateIterator(iterator.tail)(value))
    else
      Some(this)

  def get(loc: Loc): Cell = board(loc._1)(loc._2)

  def updateNeighbors(loc: Loc): Option[Sudoku] =
    get(loc) match {
      case Empty(_) => Some(this)
      case Full(num, filled) => updateIterator(neighIterator(loc))(num)
    }

  def addValue(location: Loc, value: Int, filled: Boolean = true): Option[Sudoku] = {
    affect(location, _ => Full(value, filled))
      .flatMap(_.updateNeighbors(location))
  }

  override def toString: String =
    board.map(
      row =>
        row.map(c => c.padded(this.maxPad)).mkString("[ ", " ", " ]")
    ).mkString("\n")

  def updateAll(): Option[Sudoku] =
    allLocs.foldLeft[Option[Sudoku]](Some(this))((sud, loc) =>
      sud.flatMap(_.updateNeighbors(loc))
    )

  def toResult: Result = Result(Some(this), found = false, "")

  def toSolver: Solver = Solver(Some(this), List())

}

object Sudoku {

  type Loc = (Int, Int)
  type Group = List[Loc]

  def empty: Sudoku =
    Sudoku(List.fill(9)(List.fill(9)(makeEmpty)))

  def rowIterator(loc: Loc): List[Loc] =
    List.range(0, 9).map(x => (loc._1, x))

  def colIterator(loc: Loc): List[Loc] =
    List.range(0, 9).map(x => (x, loc._2))

  def numeric(loc: Loc): Int =
    loc._1 * 9 + loc._2

  def leftCorner(loc: Loc): Loc =
    (loc._1 / 3 * 3, loc._2 / 3 * 3)

  def boxIterator(loc: Loc): List[Loc] = {
    val (x, y) = leftCorner(loc)
    List((x, y), (x, y + 1), (x, y + 2),
      (x + 1, y), (x + 1, y + 1), (x + 1, y + 2),
      (x + 2, y), (x + 2, y + 1), (x + 2, y + 2)
    )
  }

  def allLocs: List[Loc] =
    (for {
      i <- 0 to 8
      j <- 0 to 8
    } yield (i, j)).toList

  def neighIterator(loc: Loc): List[Loc] =
    (rowIterator(loc) ++ colIterator(loc) ++ boxIterator(loc)).distinct.filter(_ != loc)

  def commonNeigh(locs: List[Loc]): List[Loc] =
    locs.map(neighIterator).reduce((built, list) => built.intersect(list))

  def see(loc1: Loc, loc2: Loc): Boolean =
    loc1 != loc2 &&
      (loc1._1 == loc2._1 || loc1._2 == loc2._2 || leftCorner(loc1) == leftCorner(loc2))


  def commonGroupNeigh(grp1: Group, grp2: Group): List[Loc] =
    allLocs.filter(loc =>
      grp1.exists(p => see(p, loc)) && grp2.exists(p => see(p, loc))
    )

  def parseBoard(string: String): Option[Sudoku] = {
    makeBoard(
      string.split('\n').map(x => x.split(' ').map(_.toInt).toList).toList
    )
  }

  def makeBoard(board: List[List[Int]]): Option[Sudoku] =
    Sudoku(board.map(row => row.map(
      value => if (value != 0) Full(value, filled = false) else makeEmpty
    ))).updateAll()

}
