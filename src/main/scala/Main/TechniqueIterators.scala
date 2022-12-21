package Main

import Main.Sudoku.{allLocs, boxIterator, colIterator, commonNeigh, neighIterator, rowIterator, see}
import Main.Techniques.{Group, Loc, Rectangle}

object TechniqueIterators {

  def groups: List[Group] =
    rowGroups ++ colGroups ++ boxGroups

  def boxGroups: List[Group] =
    List.range(0, 9, 3).flatMap(row =>
      List.range(0, 9, 3).map(col =>
        (row, col)
      )
    ).map(boxIterator)

  def rowGroups: List[Group] =
    List.range(0, 9).map(x => rowIterator((x, 0)))

  def colGroups: List[Group] =
    List.range(0, 9).map(x => colIterator(0, x))

  def locAndNum: List[(Loc, Int)] =
    for {
      loc <- allLocs
      num <- 1 to 9
    } yield (loc, num)

  def groupsWithNums: List[(Group, Int)] =
    for {
      g <- groups
      i <- 1 to 9
    } yield (g, i)

  def groupsWithDouble: List[(Group, Int, Int)] =
    for {
      g <- groups
      i <- 1 to 8
      j <- (i + 1) to 9
    } yield (g, i, j)

  def groupsWithTriple: List[(Group, Int, Int, Int)] =
    for {
      g <- groups
      i <- 1 to 7
      j <- (i + 1) to 8
      k <- (j + 1) to 9
    } yield (g, i, j, k)

  def groupsWithQuadruple: List[(Group, Int, Int, Int, Int)] =
    for {
      g <- groups
      i <- 1 to 6
      j <- (i + 1) to 7
      k <- (j + 1) to 8
      l <- (k + 1) to 9
    } yield (g, i, j, k, l)

  def seeEachOther: List[(Loc, Loc)] =
    allLocs.flatMap(loc1 => neighIterator(loc1).map(loc2 => {
      val List(loc1_, loc2_) = List(loc1, loc2)
        .sortWith((a, b) => a._1 < b._1 || (a._1 == b._1 && a._2 < b._2))
      (loc1_, loc2_)
    })).distinct

  def tripleSee: List[(Loc, Loc, Loc)] =
    (for {
      loc1 <- allLocs
      loc2 <- neighIterator(loc1)
      loc3 <- commonNeigh(List(loc1, loc2))
    } yield {
      val List(loc1_, loc2_, loc3_) = List(loc1, loc2, loc3)
        .sortWith((a, b) => a._1 < b._1 || (a._1 == b._1 && a._2 < b._2))
      (loc1_, loc2_, loc3_)
    }).distinct

  def quadrupleSee: List[(Loc, Loc, Loc, Loc)] =
    (for {
      loc1 <- allLocs
      loc2 <- neighIterator(loc1)
      loc3 <- commonNeigh(List(loc1, loc2))
      loc4 <- commonNeigh(List(loc1, loc2, loc3))
    } yield {
      val List(loc1_, loc2_, loc3_, loc4_) = List(loc1, loc2, loc3, loc4)
        .sortWith((a, b) => a._1 < b._1 || (a._1 == b._1 && a._2 < b._2))
      (loc1_, loc2_, loc3_, loc4_)
    }).distinct

  def xWingIterator: List[(Int, Int, Int)] =
    (for {
      row1 <- 0 to 8
      row2 <- (row1 + 1) to 8
      value <- 1 to 9
    } yield (row1, row2, value)).toList

  def swordfishIterator: List[(Int, Int, Int, Int)] =
    (for {
      row1 <- 0 to 8
      row2 <- (row1 + 1) to 8
      row3 <- (row2 + 1) to 8
      n <- 1 to 9
    } yield (row1, row2, row3, n)).toList

  def jellyfishIterator: List[(Int, Int, Int, Int, Int)] =
    (for {
      row1 <- 0 to 8
      row2 <- (row1 + 1) to 8
      row3 <- (row2 + 1) to 8
      row4 <- (row3 + 1) to 8
      n <- 1 to 9
    } yield (row1, row2, row3, row4, n)).toList

  def YWingIterator: List[(Loc, Loc, Loc)] =
    for {
      loc <- allLocs
      neigh1 <- neighIterator(loc)
      neigh2 <- neighIterator(loc).filter(pot => !see(pot, neigh1))
    } yield (loc, neigh1, neigh2)

  def rectangleIterator: List[Rectangle] =
    (for {
      r1 <- 0 to 5
      r2 <- ((r1 / 3) * 3 + 3) to 8
      c1 <- 0 to 7
      c2 <- (c1 + 1) to ((c1 / 3) * 3 + 2)
    } yield List(((r1, c1), (r1, c2), (r2, c2), (r2, c1)), ((c1, r1), (c1, r2), (c2, r2), (c2, r1))))
      .flatten.toList

  def push[A, B, C, D]: ((A, B, C, D)) => (B, C, D, A) =
    (tup: (A, B, C, D)) => (tup._2, tup._3, tup._4, tup._1)

  def rectangleIterator4: List[Rectangle] =
    (0 to 3).foldLeft(rectangleIterator)((built, _) => rectangleIterator ++ built.map(push))

  def combineValue[A](iter: List[A]): List[(A, Int)] =
    for {
      op <- iter
      num <- 1 to 9
    } yield (op, num)

  def kiteIterator: List[(Loc, Loc, Loc, Loc, Int)] =
    for {
      loc1 <- allLocs
      loc2 <- neighIterator(loc1)
      loc3 <- neighIterator(loc2).filter(x => x != loc1 && !see(x, loc1))
      loc4 <- neighIterator(loc3).filter(x => x != loc1 && x != loc2 && !see(x, loc2))
      num <- 1 to 9
    } yield (loc1, loc2, loc3, loc4, num)

  def threeKiteIterator: List[(Loc, Loc, Loc, Loc, Loc, Loc, Int)] =
    for {
      loc1 <- allLocs
      loc2 <- neighIterator(loc1)
      loc3 <- neighIterator(loc2).filter(x => x != loc1 && !see(x, loc1))
      loc4 <- neighIterator(loc3).filter(x => x != loc1 && x != loc2 && !see(x, loc2))
      loc5 <- neighIterator(loc4).filter(x => x != loc1 && x != loc2 && x != loc3 && !see(x, loc3))
      loc6 <- neighIterator(loc5).filter(x => x != loc1 && x != loc2 && x != loc3 && x != loc4 && !see(x, loc4))
      num <- 1 to 9
    } yield (loc1, loc2, loc3, loc4, loc5, loc6, num)


  def sueDeCoqIterator: List[(Loc, Loc)] =
    for {
      loc1 <- allLocs
      loc2 <- allLocs
      if !see(loc1, loc2)
    } yield (loc1, loc2)

}
