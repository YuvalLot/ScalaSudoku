package Main

import Main.Sudoku._
import Main._Cell._
import TechniqueIterators._

import scala.annotation.tailrec

object Techniques {

  type Loc = (Int, Int)
  type Group = List[Loc]
  type Rectangle = (Loc, Loc, Loc, Loc)

  def removeFromGroup(sud: Sudoku, grp: Group, vls: List[Int], msg: String): Result = {
    val (removed, found) = grp.foldLeft[(Option[Sudoku], Boolean)]((Some(sud), false))({
      case ((Some(sud), b), loc) =>
        if (sud.get(loc).includesSome(vls))
          (sud.affect(loc, _.withoutAll(vls)), true)
        else
          (Some(sud), b)
      case ((None, b), loc) =>
        (None, b)
    })
    Result(removed, found, msg)
  }

  def onlyKeep(sudoku: Sudoku, grp: Group, vls: List[Int], msg: String): Result = {
    val (kept, found) = grp.foldLeft[(Option[Sudoku], Boolean)]((Some(sudoku), false))({
      case ((Some(sud), b), loc) =>
        if (sud.get(loc).hasBeyond(vls))
          (sud.affect(loc, _.intersect(vls)), true)
        else
          (Some(sud), b)
      case (opt, _) =>
        opt
    })
    Result(kept, found, msg)
  }

  def stronglyConnected(sud: Sudoku, num: Int, loc1: Loc, loc2: Loc): Boolean = {

    val searchFunction: Loc => Boolean =
      l => l == loc1 || l == loc2 || (!sud.get(l).includes(num))

    sud.get(loc1).includes(num) && sud.get(loc2).includes(num) &&
      (
        (loc1._1 == loc2._1 && rowIterator(loc1).forall(searchFunction)) || // same row
          (loc1._2 == loc2._2 && colIterator(loc1).forall(searchFunction)) || // same col
          (leftCorner(loc1) == leftCorner(loc2) && boxIterator(loc1).forall(searchFunction))
        )
  }

  def findOnly(sud: Sudoku, num: Int, grp: Group): List[Loc] = {
    val includesNum = grp.filter(l => sud.get(l).includes(num))
    if (includesNum.length == 1)
      includesNum
    else
      List()
  }

  def findStronglyConnected(loc: Loc, sud: Sudoku, num: Int): List[Loc] =
    if (sud.get(loc).includes(num))
      findOnly(sud, num, rowIterator(loc).filter(_ != loc)) ++
        findOnly(sud, num, colIterator(loc).filter(_ != loc)) ++
        findOnly(sud, num, boxIterator(loc).filter(_ != loc))
    else
      Nil

  def searchChain(len: Int)(built: List[Loc], num: Int, sud: Sudoku): List[List[Loc]] =
    if (built.length != len)
      (for (jump <- findStronglyConnected(built.head, sud, num))
        yield searchChain(len)(jump :: built, num, sud)).flatten
    else if (built.distinct.length == len)
      List(built)
    else
      List()


  @tailrec
  def sequentialApplication(techs: List[Technique], result: Result): Result =
    if (result.found)
      result
    else if (techs.nonEmpty)
      sequentialApplication(techs.tail, result.applyTechnique(techs.head))
    else
      result


  abstract class Technique {

    def apply(sud: Sudoku): Result

  }

  abstract class IterativeTechnique[T](iterator: List[T]) extends Technique {
    def single(t: T)(sud: Sudoku): Result

    @tailrec
    final def applyWith(result: Result, list: List[T]): Result =
      if (result.found)
        result
      else list match {
        case head :: tail => applyWith(result.flatMap(single(head)), tail)
        case Nil => result
      }

    override final def apply(sud: Sudoku): Result =
      applyWith(sud.toResult, iterator)

  }

  abstract class SingleTechnique extends IterativeTechnique[Loc](allLocs)

  abstract class ChainTechnique extends IterativeTechnique[(Loc, Int)](locAndNum)



  case object NakedSingle extends SingleTechnique {
    override def single(loc: (Int, Int))(sud: Sudoku): Result =
      sud.get(loc) match {
        case Empty(options) if options.length == 1 =>
          Result(sud.addValue(loc, options.head), found = true,
            s"Found naked single at ${loc} with value ${options.head}"
          )
        case _ => sud.toResult
      }
  }

  case object HiddenSingle extends IterativeTechnique[(Group, Int)](groupsWithNums) {
    override def single(t: (Group, Int))(sud: Sudoku): Result = {
      val (grp, vl) = t

      grp.filter (loc => sud.get(loc).includes(vl)) match {
        case List(loc) =>
          Result(sud.addValue(loc, vl), found = true,
            s"Found hidden single at ${loc} with value ${vl}"
          )
        case _ =>
          sud.toResult
      }
    }

  }

  case object NakedDouble extends IterativeTechnique[(Loc, Loc)](seeEachOther) {
    override def single(locs: ((Int, Int), (Int, Int)))(sud: Sudoku): Result = {

      val (loc1, loc2) = locs
      val (cell1, cell2) = (sud.get(loc1), sud.get(loc2))

      if (cell1.length == 2 && cell1 == cell2)
        removeFromGroup(sud, commonNeigh(List(loc1, loc2)),
          cell1.getOptions,
          s"Found Naked Double at locations $loc1, $loc2 with values ${cell1.getOptions.mkString(",")}")
      else
        sud.toResult

    }
  }

  case object HiddenDouble extends IterativeTechnique[(Group, Int, Int)](groupsWithDouble) {
    override def single(t: (Group, Int, Int))(sud: Sudoku): Result = {
      val (grp, n1, n2) = t
      val n1_locs = grp.filter(l => sud.get(l).includes(n1))
      val n2_locs = grp.filter(l => sud.get(l).includes(n2))
      if (n1_locs.length == 2 && n1_locs == n2_locs) {
        val List(loc1, loc2) = n1_locs
        if (sud.get(loc1).length > 2 || sud.get(loc2).length > 2)
          Result(
            sud.affect(loc1, _ => Empty(List(n1, n2)))
              .flatMap(_.affect(loc2, _ => Empty(List(n1, n2)))),
            found = true,
            s"Found hidden Double at location $loc1, $loc2 with values $n1, $n2")
        else
          sud.toResult
      }
      else
        sud.toResult
    }
  }

  case object PointingTuple extends IterativeTechnique[(Group, Int)](
    boxGroups.flatMap(g => List.range(1, 10).map(x => (g, x)))
  ) {
    override def single(t: (Group, Int))(sud: Sudoku): Result = {
      val (grp, value) = t
      val locs: List[Loc] = grp.filter(loc => sud.get(loc).includes(value))
      if (locs.length > 3 || locs.length < 2)
        sud.toResult
      else if (locs.forall(_._1 == locs.head._1)) {
        val row: List[Loc] = rowIterator(locs.head).filter(x => !locs.contains(x))
        removeFromGroup(sud, row, List(value),
          s"Pointing tuple at ${locs.mkString(", ")} with value $value")
      }
      else if (locs.forall(_._2 == locs.head._2)) {
        val col: List[Loc] = colIterator(locs.head).filter(x => !locs.contains(x))
        removeFromGroup(sud, col, List(value),
          s"Pointing tuple at ${locs.mkString(", ")} with value $value")
      }
      else
        sud.toResult

    }
  }

  case object LockedCandidate extends IterativeTechnique[(Group, Int)](
    (rowGroups ++ colGroups).flatMap(g => List.range(1, 10).map(x => (g, x)))
  ) {
    override def single(t: (Group, Int))(sud: Sudoku): Result = {
      val (grp, value) = t
      val locs: List[Loc] = grp.filter(loc => sud.get(loc).includes(value))
      if (locs.length > 3 || locs.length < 2)
        sud.toResult
      else if (locs.forall(loc => leftCorner(loc) == leftCorner(locs.head))) {
        val box: List[Loc] = boxIterator(locs.head).filter(x => !locs.contains(x))
        removeFromGroup(sud, box, List(value),
          s"Locked candidate at ${locs.mkString(", ")} with value $value")
      }
      else
        sud.toResult
    }
  }

  case object NakedTriple extends IterativeTechnique[(Loc, Loc, Loc)](tripleSee) {
    override def single(t: ((Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {
      val (loc1, loc2, loc3) = t
      val (cell1, cell2, cell3) = (sud.get(loc1), sud.get(loc2), sud.get(loc3))
      val union = cell1.getOptions.concat(cell2.getOptions).concat(cell3.getOptions).distinct

      if (union.length == 3 && cell1.length > 1 && cell2.length > 1 && cell3.length > 1)
        removeFromGroup(sud, commonNeigh(List(loc1, loc2, loc3)),
          union, s"Found naked triple in $loc1, $loc2, $loc3 with values ${union.mkString(", ")}")
      else
        sud.toResult
    }
  }

  case object HiddenTriple extends IterativeTechnique[(Group, Int, Int, Int)](groupsWithTriple) {
    override def single(t: (Group, Int, Int, Int))(sud: Sudoku): Result = {

      val (grp, n1, n2, n3) = t
      val vls = List(n1, n2, n3)
      val n1_locs = grp.filter(l => sud.get(l).includes(n1))
      val n2_locs = grp.filter(l => sud.get(l).includes(n2))
      val n3_locs = grp.filter(l => sud.get(l).includes(n3))
      val union = (n1_locs ++ n2_locs ++ n3_locs).distinct

      val meetsHiddenTriple: Boolean =
        union.length == 3 && n1_locs.length > 1 && n2_locs.length > 1 && n3_locs.length > 1 &&
          union.exists(loc => ! sud.get(loc).getOptions.forall(x => vls.contains(x)))

      if (meetsHiddenTriple)
        Result(
          union.foldLeft[Option[Sudoku]](Some(sud))((opt_sud, loc) =>
              opt_sud.flatMap(_.affect(loc, c => c.intersect(vls)))
          ),
          found = true,
          move = s"Found Hidden Triple at positions ${union.mkString(", ")}, with values ${vls.mkString(", ")}"
        )
      else
        sud.toResult
    }
  }

  case object NakedQuadruple extends IterativeTechnique[(Loc, Loc, Loc, Loc)](quadrupleSee) {
    override def single(t: ((Int, Int), (Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {
      val (loc1, loc2, loc3, loc4) = t
      val List(cell1, cell2, cell3, cell4) = List(loc1, loc2, loc3, loc4).map(sud.get)
      val union = List(cell1, cell2, cell3, cell4)
        .foldLeft[List[Int]](List())((built, c) => built.concat(c.getOptions))
        .distinct

      if (union.length == 4 && List(cell1, cell2, cell3, cell4).forall(_.length > 1))
        removeFromGroup(sud, commonNeigh(List(loc1, loc2, loc3, loc4)),
          union,
          msg = s"Naked Quadruple in $loc1, $loc2, $loc3, $loc4 with values ${union.sorted.mkString(", ")}")
      else
        sud.toResult
    }
  }

  case object HiddenQuadruple extends IterativeTechnique[(Group, Int, Int, Int, Int)](groupsWithQuadruple) {
    override def single(t: (Group, Int, Int, Int, Int))(sud: Sudoku): Result = {

      val (grp, n1, n2, n3, n4) = t
      val vls = List(n1, n2, n3, n4)
      val n1_locs = grp.filter(l => sud.get(l).includes(n1))
      val n2_locs = grp.filter(l => sud.get(l).includes(n2))
      val n3_locs = grp.filter(l => sud.get(l).includes(n3))
      val n4_locs = grp.filter(l => sud.get(l).includes(n4))
      val union = (n1_locs ++ n2_locs ++ n3_locs ++ n4_locs).distinct

      val meetsHiddenTriple: Boolean =
        union.length == 4 && n1_locs.length > 1 && n2_locs.length > 1 && n3_locs.length > 1 &&
          n4_locs.length > 1 &&
          union.exists(loc => ! sud.get(loc).getOptions.forall(x => vls.contains(x)))

      if (meetsHiddenTriple)
        Result(
          union.foldLeft[Option[Sudoku]](Some(sud))((opt_sud, loc) =>
            opt_sud.flatMap(_.affect(loc, c => c.intersect(vls)))
          ),
          found = true,
          move = s"Found Hidden Triple at positions ${union.mkString(", ")}, with values ${vls.mkString(", ")}"
        )
      else
        sud.toResult
    }
  }

  case object XWing extends IterativeTechnique[(Int, Int, Int)](xWingIterator) {
    override def single(t: (Int, Int, Int))(sud: Sudoku): Result = {
      val (index1, index2, value) = t

      val row1_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index1, col)).includes(value))
      val row2_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index2, col)).includes(value))
      val col1_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index1)).includes(value))
      val col2_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index2)).includes(value))

      val using_rows =
        if (row1_loc.length == 2 && row1_loc == row2_loc)
          removeFromGroup(sud,
            grp = colIterator((index1, row1_loc.head))
                    .concat(colIterator(index1, row1_loc(1)))
                    .filter(x => x._1 != index2 && x._1 != index1),
            vls = List(value),
            msg = s"X-Wing at rows $index1, $index2 with value $value"
          )
        else
          sud.toResult

      using_rows.flatMap(sud =>
        if (col1_loc.length == 2 && col1_loc == col2_loc)
          removeFromGroup(sud,
            grp = (rowIterator((col1_loc.head, index2))
                    ++ rowIterator(col1_loc(1), index2))
                    .filter(x => x._2 != index1 && x._2 != index2),
            vls = List(value),
            msg = s"X-Wing at cols $index1, $index2 with value $value"
          )
        else
          sud.toResult
      )

    }
  }

  case object YWing extends IterativeTechnique[(Loc, Loc, Loc)](YWingIterator) {
    override def single(t: ((Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {
      val (loc, neigh1, neigh2) = t
      val (loc_cell, neigh1_cell, neigh2_cell) = (sud.get(loc), sud.get(neigh1), sud.get(neigh2))

      if (meetsYWing(loc_cell, neigh1_cell, neigh2_cell))
        removeFromGroup(sud,
          grp = commonNeigh(List(neigh1, neigh2))
            .filter(x => x != neigh1 && x != neigh2 && x != loc),
          vls = neigh1_cell.getOptions.intersect(neigh2_cell.getOptions),
          msg = s"Found YWing at positions $loc with neighbors $neigh1, $neigh2"
        )
      else
        sud.toResult
    }

    def meetsYWing(cell: Cell, neigh1: Cell, neigh2: Cell): Boolean =
      cell.length == 2 && neigh1.length == 2 && neigh2.length == 2 &&
        (cell.getOptions.intersect(neigh1.getOptions).length == 1) &&
        (cell.getOptions.intersect(neigh2.getOptions).length == 1) &&
        (neigh1.getOptions.intersect(neigh2.getOptions).length == 1) &&
        (cell.getOptions.intersect(neigh1.getOptions).intersect(neigh2.getOptions).isEmpty)
  }

  case object XYZWing extends IterativeTechnique[(Loc, Loc, Loc)](YWingIterator) {

    override def single(t: ((Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {
      val (pivot, neigh1, neigh2) = t
      val (pivot_cell, neigh1_cell, neigh2_cell) =
        (sud.get(pivot), sud.get(neigh1), sud.get(neigh2))

      val intersection = neigh1_cell.getOptions.intersect(neigh2_cell.getOptions)

      val meetsXYZ: Boolean =
        pivot_cell.length == 3 && neigh1_cell.length == 2 && neigh2_cell.length == 2 &&
          pivot_cell.getOptions.intersect(neigh1_cell.getOptions).length == 2 &&
          pivot_cell.getOptions.intersect(neigh2_cell.getOptions).length == 2 &&
          intersection.length == 1

      if (meetsXYZ)
        removeFromGroup(sud, commonNeigh(List(pivot, neigh1, neigh2)),
          vls = intersection,
          msg = s"XYZ-Wing with pivot $pivot, wings $neigh1, $neigh2 and " +
            s"common value ${intersection.head}")
      else
        sud.toResult
    }

  }

  case object Swordfish extends IterativeTechnique[(Int, Int, Int, Int)](swordfishIterator) {
    override def single(t: (Int, Int, Int, Int))(sud: Sudoku): Result = {
      val (ind1, ind2, ind3, num) = t

      val row1_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind1, col)).includes(num))
      val row2_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind2, col)).includes(num))
      val row3_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind3, col)).includes(num))
      val row_union = (row1_loc ++ row2_loc ++ row3_loc).distinct

      lazy val col1_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind1)).includes(num))
      lazy val col2_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind2)).includes(num))
      lazy val col3_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind3)).includes(num))
      lazy val col_union = (col1_loc ++ col2_loc ++ col3_loc).distinct

      val using_rows: Result =
        if (List(row1_loc.length, row2_loc.length, row3_loc.length).forall(x => x > 1 && x < 4) &&
          row_union.length == 3)
          removeFromGroup(
            sud,
            grp = colIterator((ind1, row_union.head))
              .concat(colIterator((ind1, row_union(1))))
              .concat(colIterator((ind1, row_union(2))))
              .filter(x => x._1 != ind1 && x._1 != ind2 && x._1 != ind3),
            vls = List(num),
            msg = s"Swordfish at rows $ind1, $ind2, $ind3 with value $num"
          )
        else
          sud.toResult

      using_rows.flatMap(sud =>
        if (List(col1_loc.length, col2_loc.length, col3_loc.length).forall(x => x > 1 && x < 4) &&
          col_union.length == 3)
          removeFromGroup(
            sud,
            grp = rowIterator((col_union.head, ind1))
              .concat(rowIterator((col_union(1), ind1)))
              .concat(rowIterator((col_union(2), ind1)))
              .filter(x => x._2 != ind1 && x._2 != ind2 && x._2 != ind3),
            vls = List(num),
            msg = s"Swordfish at cols $ind1, $ind2, $ind3 with value $num"
          )
        else
          sud.toResult
      )
    }
  }

  case object Jellyfish extends IterativeTechnique[(Int, Int, Int, Int, Int)](jellyfishIterator) {
    override def single(t: (Int, Int, Int, Int, Int))(sud: Sudoku): Result = {
      val (ind1, ind2, ind3, ind4, num) = t

      val row1_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind1, col)).includes(num))
      val row2_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind2, col)).includes(num))
      val row3_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind3, col)).includes(num))
      val row4_loc: List[Int] = List.range(0, 9).filter(col => sud.get((ind4, col)).includes(num))
      val row_union = (row1_loc ++ row2_loc ++ row3_loc ++ row4_loc).distinct

      lazy val col1_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind1)).includes(num))
      lazy val col2_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind2)).includes(num))
      lazy val col3_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind3)).includes(num))
      lazy val col4_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, ind4)).includes(num))
      lazy val col_union = (col1_loc ++ col2_loc ++ col3_loc ++ col4_loc).distinct

      val using_rows: Result =
        if (List(row1_loc.length, row2_loc.length, row3_loc.length, row4_loc.length)
                          .forall(x => x > 1 && x < 5) &&
            row_union.length == 4)
          removeFromGroup(
            sud,
            grp = colIterator((ind1, row_union.head))
              .concat(colIterator((ind1, row_union(1))))
              .concat(colIterator((ind1, row_union(2))))
              .concat(colIterator((ind1, row_union(3))))
              .filter(x => x._1 != ind1 && x._1 != ind2 && x._1 != ind3 && x._1 != ind4),
            vls = List(num),
            msg = s"Jellyfish at rows $ind1, $ind2, $ind3, $ind4 with value $num"
          )
        else
          sud.toResult

      using_rows.flatMap(sud =>
        if (List(col1_loc.length, col2_loc.length, col3_loc.length, col4_loc.length)
                  .forall(x => x > 1 && x < 4) &&
            col_union.length == 4)
          removeFromGroup(
            sud,
            grp = rowIterator((col_union.head, ind1))
              .concat(rowIterator((col_union(1), ind1)))
              .concat(rowIterator((col_union(2), ind1)))
              .concat(rowIterator((col_union(3), ind1)))
              .filter(x => x._2 != ind1 && x._2 != ind2 && x._2 != ind3 && x._2 != ind4),
            vls = List(num),
            msg = s"Jellyfish at cols $ind1, $ind2, $ind3, $ind4 with value $num"
          )
        else
          sud.toResult
      )

    }
  }



  case object FinnedXWing extends IterativeTechnique[(Int, Int, Int)](xWingIterator) {

    override def single(t: (Int, Int, Int))(sud: Sudoku): Result = {
      val (index1, index2, value) = t

      val row1_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index1, col)).includes(value))
      val row2_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index2, col)).includes(value))

      val usingRows =
        if (List(row1_loc.length, row2_loc.length).sorted != List(2, 3))
          sud.toResult
        else if (row1_loc.forall(row2_loc.contains))
          rowSingle(t, row1_loc, row2_loc)(sud)
        else if (row2_loc.forall(row1_loc.contains))
          rowSingle((index2, index1, value), row2_loc, row1_loc)(sud)
        else
          sud.toResult

      lazy val col1_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index1))
        .includes(value))
      lazy val col2_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index2))
        .includes(value))

      lazy val usingCols =
        if (List(col1_loc.length, col2_loc.length).sorted != List(2, 3))
          sud.toResult
        else if (col1_loc.forall(col2_loc.contains))
          colSingle(t, col1_loc, col2_loc)(sud)
        else if (col2_loc.forall(col1_loc.contains))
          colSingle((index2, index1, value), col2_loc, col1_loc)(sud)
        else
          sud.toResult

      usingRows.flatMap(_ => usingCols)
    }

    def rowSingle(t: (Int, Int, Int), row1_loc: List[Int], row2_loc: List[Int])(sud: Sudoku): Result = {
      val (index1, index2, value) = t
      val problematic = (index2, row2_loc.filter(x => !row1_loc.contains(x)).head)
      val seen: List[Loc] =
        colIterator((index1, row1_loc.head))
          .concat(colIterator(index1, row1_loc(1)))
          .filter(x => x._1 != index2 && x._1 != index1)
          .intersect(neighIterator(problematic))

      removeFromGroup(sud, seen, List(value),
        msg = s"Finned XWing at rows $index1, $index2, with value $value")
    }

    def colSingle(t: (Int, Int, Int), col1_loc: List[Int], col2_loc: List[Int])(sud: Sudoku): Result = {
      val (index1, index2, value) = t
      val problematic = (col2_loc.filter(x => !col1_loc.contains(x)).head, index2)
      val seen: List[Loc] =
        rowIterator((col1_loc.head, index1))
          .concat(rowIterator(col1_loc(1), index1))
          .filter(x => x._2 != index2 && x._2 != index1)
          .intersect(neighIterator(problematic))

      removeFromGroup(sud, seen, List(value),
        msg = s"Finned XWing at cols $index1, $index2, with value $value")
    }

  }




  case object TwoKite extends IterativeTechnique[(Loc, Loc, Loc, Loc, Int)](kiteIterator) {

    override def single(t: ((Int, Int), (Int, Int), (Int, Int), (Int, Int), Int))(sud: Sudoku): Result = {
      val (l1, m1, l2, m2, vl) = t

      if (stronglyConnected(sud, vl, l1, m1) && stronglyConnected(sud, vl, m1, l2)  &&
          stronglyConnected(sud, vl, l2, m2))
        removeFromGroup(sud, commonGroupNeigh(List(l1, l2), List(m1, m2)), List(vl),
          msg = s"2-String Kite at loc $l1, $m1, $l2, $m2 with value $vl")
      else
        sud.toResult
    }

  }

  case object ThreeKite extends IterativeTechnique[(Loc, Loc, Loc, Loc, Loc, Loc, Int)](threeKiteIterator) {

    override def single(t: (Loc, Loc, Loc, Loc, Loc, Loc, Int))(sud: Sudoku): Result = {
      val (l1, m1, l2, m2, l3, m3, vl) = t

      if (stronglyConnected(sud, vl, l1, m1) && stronglyConnected(sud, vl, m1, l2)  &&
        stronglyConnected(sud, vl, l2, m2) && stronglyConnected(sud, vl, m2, l3) &&
        stronglyConnected(sud, vl, l3, m3))
        removeFromGroup(sud, commonGroupNeigh(List(l1, l2, l3), List(m1, m2, m3)), List(vl),
          msg = s"3-String Kite at locations $l1, $m1, $l2, $m2, $l3, $m3")
      else
        sud.toResult
    }

  }



  case object TwoStringKite extends ChainTechnique {

    def search: (List[(Int, Int)], Int, Sudoku) => List[List[(Int, Int)]] = searchChain(len = 4)

    override def single(t: ((Int, Int), Int))(sud: Sudoku): Result = {
      val (loc, num) = t
      search(List(loc), num, sud)
        .foldLeft[Result](sud.toResult)((resulted, chain) =>
          if (resulted.found)
            resulted
          else {
            val List(l1, m1, l2, m2) = chain
            if (stronglyConnected(sud, num, l1, m1) && stronglyConnected(sud, num, m1, l2)  &&
                stronglyConnected(sud, num, l2, m2))
              removeFromGroup(sud, commonGroupNeigh(List(l1, l2), List(m1, m2)), List(num),
                msg = s"2-String Kite at locations $l1, $m1, $l2, $m2 with value $num")
            else
              sud.toResult
          }
        )
    }
  }

  case object ColorChain extends ChainTechnique {

    def search: (List[(Int, Int)], Int, Sudoku) => List[List[(Int, Int)]] = searchChain(len = 6)

    override def single(t: ((Int, Int), Int))(sud: Sudoku): Result = {
      val (loc, num) = t
      search(List(loc), num, sud)
        .foldLeft[Result](sud.toResult)((resulted, chain) =>
          if (resulted.found)
            resulted
          else {
            val List(l1, m1, l2, m2, l3, m3) = chain
            if (stronglyConnected(sud, num, l1, m1) && stronglyConnected(sud, num, m1, l2)  &&
              stronglyConnected(sud, num, l2, m2) && stronglyConnected(sud, num, m2, l3) &&
              stronglyConnected(sud, num, l3, m3))
              removeFromGroup(sud, commonGroupNeigh(List(l1, l2, l3), List(m1, m2, m3)), List(num),
                msg = s"Color Chain at locations $l1, $m1, $l2, $m2, $l3, $m3 with value $num")
            else
              sud.toResult
          }
        )
    }
  }



  case object Skyscraper extends IterativeTechnique[(Int, Int, Int)](xWingIterator) {
    override def single(t: (Int, Int, Int))(sud: Sudoku): Result =
      singleRow(t)(sud).flatMap(_ => singleColumn(t)(sud))

    def singleRow(t: (Int, Int, Int))(sud: Sudoku): Result = {
      val (index1, index2, value) = t

      val row1_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index1, col)).includes(value))
      val row2_loc: List[Int] = List.range(0, 9).filter(col => sud.get((index2, col)).includes(value))

      lazy val row1_wo_row2 = row1_loc.filter(!row2_loc.contains(_))
      lazy val row2_wo_row1 = row2_loc.filter(!row1_loc.contains(_))

      if (row1_loc.length == 2 && row2_loc.length == 2 &&
          row1_wo_row2.length == 1 && row2_wo_row1.length == 1)
        removeFromGroup(sud,
          commonNeigh(List((index1, row1_wo_row2.head), (index2, row2_wo_row1.head))),
          vls = List(value),
          msg = s"Skyscraper at rows $index1, $index2, with value $value")
      else
        sud.toResult
    }

    def singleColumn(t: (Int, Int, Int))(sud: Sudoku): Result = {
      val (index1, index2, value) = t

      val col1_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index1)).includes(value))
      val col2_loc: List[Int] = List.range(0, 9).filter(row => sud.get((row, index2)).includes(value))

      lazy val col1_wo_col2 = col1_loc.filter(!col2_loc.contains(_))
      lazy val col2_wo_col1 = col2_loc.filter(!col1_loc.contains(_))

      if (col1_loc.length == 2 && col2_loc.length == 2 &&
        col2_wo_col1.length == 1 && col1_wo_col2.length == 1)
        removeFromGroup(sud,
          commonNeigh(List((col1_wo_col2.head, index1), (col2_wo_col1.head, index2))),
          vls = List(value),
          msg = s"Sashimi Finned XWing at cols $index1, $index2, with value $value")
      sud.toResult
    }
  }

  case object SueDeCoqType1 extends IterativeTechnique[(Loc, Loc)](sueDeCoqIterator) {
    override def single(t: (Loc, Loc))(sud: Sudoku): Result = {
      val (loc1, loc2) = t
      val (cell1, cell2) = (sud.get(loc1), sud.get(loc2))

      if (cell1.length != 2 || cell2.length != 2 ||
        cell1.getOptions.intersect(cell2.getOptions).nonEmpty)
        sud.toResult
      else findCommon(loc1, loc2, cell1, cell2, sud) match {
        case lst if lst.length == 2 =>
          removeFromGroup(sud, commonNeigh(loc1 :: lst), cell1.getOptions, msg = "")
            .andAlso(sud2 =>
              removeFromGroup(sud2, commonNeigh(loc2 :: lst), cell2.getOptions,
                msg = s"Sue de Coq (t1) in locations $loc1, $loc2, with common $lst"
              )
            )

        case _ =>
          sud.toResult
      }
    }

    def findCommon(loc1: Loc, loc2: Loc, cell1: Cell, cell2: Cell, sud: Sudoku): List[Loc] =
      commonNeigh(List(loc1, loc2)).filter(loc => sud.get(loc).isEmpty &&
        sud.get(loc).getOptions.forall(option => cell1.includes(option) || cell2.includes(option))
      )
  }

  case object SueDeCoqType2 extends IterativeTechnique[(Loc, Loc)](sueDeCoqIterator) {
    override def single(t: (Loc, Loc))(sud: Sudoku): Result = {
      val (loc1, loc2) = t
      val (cell1, cell2) = (sud.get(loc1), sud.get(loc2))
      lazy val common = rowIterator(loc1).intersect(boxIterator(loc2)).filter(x => x != loc1 && x != loc2) ++
        colIterator(loc1).intersect(boxIterator(loc2)).filter(x => x != loc1 && x != loc2)
      lazy val valueUnion = common.flatMap(loc => sud.get(loc).getOptions).distinct
      lazy val twoTwosie = cell1.getOptions ++ cell2.getOptions
      lazy val other = valueUnion.filter(!twoTwosie.contains(_))

      if (cell1.length != 2 || cell2.length != 2 ||
          cell1.getOptions.intersect(cell2.getOptions).nonEmpty || common.length != 3 ||
          valueUnion.length != 5 || other.length != 1)
        sud.toResult
      else
        removeFromGroup(sud, commonNeigh(loc1 :: common), other ++ cell1.getOptions, msg = "")
          .andAlso(sud2 =>
            removeFromGroup(sud2, commonNeigh(loc2 :: common), other ++ cell2.getOptions,
              msg = s"Sue de Coq (t2) in locations $loc1, $loc2, with common $common"
            )
          )
    }
  }



  case object AvoidableRectangle extends IterativeTechnique[Rectangle](rectangleIterator4) {
    override def single(t: ((Int, Int), (Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t

      (sud.get(cor1), sud.get(cor2), sud.get(cor3), sud.get(cor4)) match {
        case (Full(x1, true), Full(y, true), Full(x2, true), Empty(lst))
          if x1 == x2 && lst.contains(y) && lst.length == 2 =>
            removeFromGroup(sud, List(cor4), List(y),
              msg = s"Avoidable Rectangle at locations $cor1, $cor2, $cor3, $cor4 with values $x1, $y")

        case _ => sud.toResult
      }
    }
  }

  case object UniqueRectangleType1 extends IterativeTechnique[Rectangle](rectangleIterator) {
    override def single(t: Rectangle)(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t
      val List(cell1, cell2, cell3, cell4) = List(cor1, cor2, cor3, cor4).map(sud.get)
      val before_cell_list = List(cell1, cell2, cell3, cell4)
      val cell_list = Set(cell1, cell2, cell3, cell4).toList

      // if (cor1 == (0, 0) && cor2 == (0, 7) && cor3 == (1, 7))
      //   println(cor1, cor2, cor3, cor4, cell_list)

      // we need 3/4 of the rectangles to be the same, and have only 2 options
      if (cell_list.size == 2 && (cell_list.head.length == 2 || cell_list.last.length == 2)
            && before_cell_list.foldLeft(cell_list.map(x => x -> 0).toMap)((mp, c)
                  => mp.updated(c, mp(c) + 1)).values.toSet == Set(1, 3)) {
        val twosie: List[Int] =
          if (cell_list.head.length == 2)
            cell_list.head.getOptions
          else
            cell_list.last.getOptions

        if (cell_list.exists(_.hasBeyond(twosie)))
          removeFromGroup(sud,
            List(cor1, cor2, cor3, cor4).filter(loc => sud.get(loc).getOptions != twosie),
            vls = twosie,
            msg = s"Unique Rectangle (t1) at $cor1, $cor2, $cor3, $cor4 with values ${twosie.mkString(", ")}"
          )
        else
          sud.toResult
      } else
        sud.toResult
    }
  }

  case object UniqueRectangleType2 extends IterativeTechnique[Rectangle](rectangleIterator4) {
    override def single(t: Rectangle)(sud: Sudoku): Result = {
      val (cor1, _, cor3, _) = t
      val (cell1, cell3) = (sud.get(cor1), sud.get(cor3))
      (for (v1 <- cell1.getOptions; v2 <- cell3.getOptions; if v1 != v2) yield (v1, v2))
        .foldLeft[Result](sud.toResult)((res, v1v2) =>
          res.flatMap(_ => singleWithValue(t, v1v2._1, v1v2._2)(sud))
        )
    }

    def singleWithValue(t: Rectangle, v1: Int, v2: Int)(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t
      if (stronglyConnected(sud, v1, cor4, cor1) && stronglyConnected(sud, v2, cor1, cor2) &&
          stronglyConnected(sud, v1, cor2, cor3))
        removeFromGroup(sud, List(cor3, cor4), List(v2),
          msg = s"Unique Rectangle (t2) at $cor1, $cor2, $cor3, $cor4 with values $v1, $v2")
      else
        sud.toResult
    }
  }

  case object UniqueRectangleType3 extends IterativeTechnique[Rectangle](rectangleIterator4) {

    override def single(t: Rectangle)(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t
      val cell_list = List(cor1, cor2, cor3, cor4).map(sud.get)
      val List(cell1, cell2, cell3, cell4) = cell_list

      if (meetsConditions(cell1, cell2, cell3, cell4)) {
        val removed = cell3.getOptions.filter(!cell1.includes(_))
        assert(removed.length == 1)
        removeFromGroup(sud, commonNeigh(List(cor3, cor4).filter(x => x != cor1 && x != cor2)), removed,
          msg = s"Unique Rectangle (t3) at $cor1, $cor2, $cor3, $cor4 " +
            s"with common values ${cell1.getOptions.mkString(", ")} " +
            s"and removed value ${removed.head}")
      }
      else
        sud.toResult

    }

    def meetsConditions(cell1: Cell, cell2: Cell, cell3: Cell, cell4: Cell): Boolean =
      cell1.length == 2 && cell1 == cell2 && cell3.includes(cell1) && cell4.includes(cell1) &&
        cell3.length == 3 && cell3 == cell4

  }

  case object HiddenRectangleType1 extends IterativeTechnique[Rectangle](rectangleIterator4) {
    override def single(t: Rectangle)(sud: Sudoku): Result =
      if (sud.get(t._1).length != 2)
        sud.toResult
      else {
        val List(v1, v2) = sud.get(t._1).getOptions
        singValue(t, v1, v2)(sud).flatMap(_ => singValue(t, v2, v1)(sud))
      }

    def singValue(t: Rectangle, pivot: Int, removed: Int)(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t
      if (stronglyConnected(sud, pivot, cor2, cor3) && stronglyConnected(sud, pivot, cor4, cor3))
        removeFromGroup(sud, List(cor3), List(removed),
          msg = s"HiddenRectangle (t1) in $cor1, $cor2, $cor3, $cor4, with pivot $pivot and removed $removed")
      else
        sud.toResult
    }
  }

  case object HiddenRectangleType2 extends IterativeTechnique[(Loc, Loc, Loc, Loc)](rectangleIterator4) {
    override def single(t: ((Int, Int), (Int, Int), (Int, Int), (Int, Int)))(sud: Sudoku): Result = {

      if (t == ((7,4), (8,4), (8,1), (7,1)))
        println("Got to where it was supposed to get")

      sud.get(t._1) match {
        case Empty(List(x, y)) =>
          singleWithValue(t, x, sud).flatMap(_ => singleWithValue(t, y, sud))
        case _ =>
          sud.toResult
      }
    }

    def singleWithValue(r: Rectangle, feature: Int, sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = r
      if (sud.get(cor1).length == 2 && sud.get(cor1) == sud.get(cor2) &&
        stronglyConnected(sud, feature, cor1, cor2) && stronglyConnected(sud, feature, cor2, cor3) &&
        stronglyConnected(sud, feature, cor3, cor4) // && stronglyConnected(sud, feature, cor4, cor1)
      )
        removeFromGroup(sud, List(cor3, cor4), sud.get(cor1).getOptions.filter(_ != feature),
          msg = s"Hidden Rectangle (t2) at locations $cor1, $cor2, $cor3, $cor4 with feature $feature")
      else
        sud.toResult
    }
  }

  case object PointingRectangle extends IterativeTechnique[Rectangle](rectangleIterator4) {
    override def single(t: Rectangle)(sud: Sudoku): Result = {
      val (cor1, cor2, cor3, cor4) = t

      searchSupport(cor1, cor2, cor3, cor4, sud) match {
        case Some((pivot, pair)) =>
          removeFromGroup(sud, commonNeigh(List(cor3, cor4, pivot)), pair,
            msg = s"Pointing Rectangle, location $cor1, $cor2, $cor3, $cor4, pivot at $pivot, deleted values ${pair.mkString(", ")}")

        case None =>
          sud.toResult
      }
    }

    def searchSupport(loc1: Loc, loc2: Loc, loc3: Loc, loc4: Loc, sud: Sudoku): Option[(Loc, List[Int])] = {
      val List(cell1, cell2, cell3, cell4) = List(loc1, loc2, loc3, loc4).map(sud.get)
      if (cell1.length == 2 && cell1 == cell2 && cell3.includes(cell1) && cell4.includes(cell1) &&
        cell3.length == 3 && cell4.length == 3) {

        val searchPair = (cell3.getOptions ++ cell4.getOptions).filter(num => !cell1.includes(num))
          .distinct.sorted

        if (searchPair.length != 2)
          None
        else
          commonNeigh(List(loc3, loc4)).collectFirst({
            case loc if sud.get(loc).getOptions.sorted == searchPair => (loc, searchPair)
          })

      } else None
    }

  }


  case class OrderedSolver(techs: List[Technique]) extends Technique {
    override def apply(sud: Sudoku): Result =
      sequentialApplication(techs, sud.toResult)
  }

}

