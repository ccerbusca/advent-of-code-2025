package day4

import scala.annotation.tailrec
import scala.util.chaining.*

val coordinates = {
  val set = for {
    a <- Set(0, -1, 1)
    b <- Set(0, -1, 1)
  } yield (a, b)
  set.excl((0, 0))
}

def checkSurroundings(
  occupiedCells: Set[(Int, Int)],
) =
  occupiedCells.filter { case (i, j) =>
    coordinates
      .map { case (x, y) => (i + x) -> (j + y) }
      .intersect(occupiedCells)
      .size < 4
  }

def part1(): Unit =
  val source = io.Source.fromResource("day4.txt")
  val mat = source
    .getLines()
    .to(LazyList)
    .map { line =>
      line.toVector
    }
    .toVector

  val occupiedCells = mat.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) { case (set, (row, i)) =>
    val rowSet = row.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) { case (rowSet, (cell, j)) =>
      if cell == '@' then rowSet.incl((i, j)) else rowSet
    }
    set ++ rowSet
  }

  source.close()
  
  checkSurroundings(
    occupiedCells = occupiedCells,
  ).size.tap(println)

@tailrec
def checkRemovable(
  occupiedCells: Set[(Int, Int)],
  totalCount: Int = 0
): Int =
  val removable = checkSurroundings(occupiedCells)
  if removable.nonEmpty then
    checkRemovable(
      occupiedCells.diff(removable),
      totalCount + removable.size
    )
  else
    totalCount

def part2(): Unit =
  val source = io.Source.fromResource("day4.txt")
  val mat = source
    .getLines()
    .to(LazyList)
    .map { line =>
      line.toVector
    }
    .toVector

  val occupiedCells = mat.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) { case (set, (row, i)) =>
    val rowSet = row.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) { case (rowSet, (cell, j)) =>
      if cell == '@' then rowSet.incl((i, j)) else rowSet
    }
    set ++ rowSet
  }

  source.close()

  checkRemovable(
    occupiedCells = occupiedCells,
  ).tap(println)

@main
def main(): Unit =
  part2()