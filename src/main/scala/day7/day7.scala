package day7

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.chaining.*

def valid(
  i: Int,
  j: Int,
  iMax: Int,
  jMax: Int,
): Boolean =
  i >= 0 && i < iMax && j >= 0 && j < jMax

@tailrec
def traverseBeam(
  q: Queue[(Int, Int)],
  splitters: Set[(Int, Int)],
  iMax: Int,
  jMax: Int,
  visited: Set[(Int, Int)] = Set.empty,
  splitCount: Int = 0,
): Int =
  q.dequeueOption match {
    case None =>
      splitCount
    case Some(((i, j), rest)) if valid(i, j, iMax, jMax) && !visited.contains((i, j)) =>
      if splitters.contains((i, j)) then
        traverseBeam(
          rest.enqueue((i, j - 1)).enqueue((i, j + 1)),
          splitters,
          iMax,
          jMax,
          visited.incl((i, j)),
          splitCount + 1,
        )
      else
        traverseBeam(
          rest.enqueue((i + 1, j)),
          splitters,
          iMax,
          jMax,
          visited.incl((i, j)),
          splitCount,
        )
    case Some(_, rest) =>
      traverseBeam(
        rest,
        splitters,
        iMax,
        jMax,
        visited,
        splitCount,
      )
  }


def solve(): Unit =
  val source = io.Source.fromResource("day7.txt")
  val input = source
    .getLines()
    .takeWhile(_.nonEmpty)
    .to(Vector)

  val startJ = input.head.indexOf('S')

  val splitterCoordinates = input
    .drop(1)
    .zipWithIndex
    .foldLeft(Set.empty[(Int, Int)]) { case (coords, (line, i)) =>
      val lineCoords =
        line
          .zipWithIndex
          .foldLeft(Set.empty[(Int, Int)]) { case (lineCoords, (cell, j)) =>
            if cell == '^' then lineCoords.incl((i + 1, j)) else lineCoords
          }
      coords ++ lineCoords
    }

//  PART1
//  traverseBeam(
//    q = Queue((1, startJ)),
//    splitters = splitterCoordinates,
//    iMax = input.length,
//    jMax = input.head.length,
//  )

  traverseTimelines(
    startJ = startJ,
    splitters = splitterCoordinates.groupMap(_._1)(_._2).withDefaultValue(Set.empty),
    lineCount = input.length
  ).tap(println)


// part2
def traverseTimelines(
  startJ: Int,
  splitters: Map[Int, Set[Int]],
  lineCount: Int,
): Long = {
  (1 until lineCount).foldLeft(Map(startJ -> 1L)) { case (branches, i) =>
    splitters(i).foldLeft(branches) { case (newBranches, j) =>
      val currentBranch = newBranches.getOrElse(j, 0L)
      newBranches
        .updatedWith(j - 1)(_.map(_ + currentBranch).orElse(Some(currentBranch)))
        .updatedWith(j + 1)(_.map(_ + currentBranch).orElse(Some(currentBranch)))
        .removed(j)
    }
  }
    .values
    .sum
}

@main
def main(): Unit =
  solve()