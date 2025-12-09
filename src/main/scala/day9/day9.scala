package day9

import scala.util.chaining.*

type Point = (x: Long, y: Long)
type Edge = (Point, Point)

def area(
  a: Point,
  b: Point,
): Long =
  (a, b) match {
    case ((x1, y1), (x2, y2)) =>
      val lines = math.abs(x1 - x2) + 1
      val columns = math.abs(y1 - y2) + 1
      lines * columns
  }

def solve(
  file: String,
): Unit =
  val source = io.Source.fromResource(file)
  val input = source
    .getLines()
    .takeWhile(_.nonEmpty)
    .map { line =>
      val split = line.split(",")
      split(0).toLong -> split(1).toLong
    }
    .to(List)
    .combinations(2).map(l => l.head -> l(1))
    .map(area)
    .max
    .tap(println)


@main
def main(): Unit = {
  // PART1
  solve("day9_2.txt")
}