package day5

import scala.util.chaining.*

case class Range(start: Long, end: Long) {
  def contains(a: Long): Boolean =
    a >= start && a <= end
    
  def overlaps(that: Range): Boolean =
    start <= that.end && that.start <= end
    
  def combine(that: Range): Range =
    Range(
      math.min(start, that.start),
      math.max(end, that.end)
    )

  def size: Long =
    end - start + 1
}

def part1(): Unit =
  val source = io.Source.fromResource("day5.txt")
  val (rangeLines, ingredientLines) = source
    .getLines()
    .to(LazyList)
    .span(_.nonEmpty)

  val ranges = rangeLines
    .map { line =>
      val s = line.split('-')
      Range(s(0).toLong, s(1).toLong)
    }
    .toList

  val result = ingredientLines
    .drop(1)
    .map(_.toLong)
    .count(ingredientId =>
      ranges.exists(_.contains(ingredientId))
    )

  source.close()
  println(result)

def part2(): Unit =
  val source = io.Source.fromResource("day5.txt")
  val ranges = source
    .getLines()
    .to(LazyList)
    .takeWhile(_.nonEmpty)
    .map { line =>
      val s = line.split('-')
      Range(s(0).toLong, s(1).toLong)
    }
    .sortBy(_.start)
    .foldLeft(List.empty[Range]) { case (processed, range) =>
      processed match {
        case p :: rest =>
          if range.overlaps(p) then range.combine(p) :: rest
          else range :: processed
        case Nil =>
          List(range)
      }
    }
    .map(_.size)
    .sum
    .tap(println)
  

  source.close()

@main
def main(): Unit =
  part2()