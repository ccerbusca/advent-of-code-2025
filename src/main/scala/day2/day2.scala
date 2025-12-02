package day2

import scala.util.chaining.*

case class Range(start: Long, end: Long)

def part1(): Unit =
  val source = io.Source.fromResource("day2.txt")
  val res = source
    .getLines()
    .next()
    .split(",")
    .toList
    .map { in =>
      val ranges = in.split("-")
      Range(ranges(0).toLong, ranges(1).toLong)
    }
    .flatMap { r =>
      LazyList.range(r.start, r.end + 1L)
        .filter { i =>
          val str = i.toString
          str.length % 2 == 0 && str.splitAt(str.length / 2).pipe(_ == _)
        }
        .toList
    }
    .sum

  source.close()
  println(res)

def part2(): Unit =
  def divisors(n: Int) =
    1 :: List.range(2, n).filter(n % _ == 0)

  def invalidId(id: String): Boolean =
    id.length > 1 && divisors(id.length).exists { div =>
      id.grouped(div).toSet.size == 1
    }

  val source = io.Source.fromResource("day2.txt")
  val res = source
    .getLines()
    .next()
    .split(",")
    .toList
    .map { in =>
      val ranges = in.split("-")
      Range(ranges(0).toLong, ranges(1).toLong)
    }
    .flatMap { r =>
      LazyList.range(r.start, r.end + 1L)
        .filter { i =>
          invalidId(i.toString)
        }
        .toList
    }
    .sum

  source.close()
  println(res)


@main
def main(): Unit =
  part2()