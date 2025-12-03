package day3

import scala.util.chaining.*


def part1(): Unit =
  val source = io.Source.fromResource("day3.txt")
  val res = source
    .getLines()
    .to(LazyList)
    .map { line =>
      val one :: two :: rest = line.toList.map(_.toString.toInt).tap(println)
      rest.foldLeft((one, two)) { case ((max1, max2), e) =>
        if (max1 < max2) {
          (max2, e)
        } else if (e > max2) {
          (max1.max(max2), e)
        } else {
          (max1, max2)
        }
      }.pipe { case (a, b) => s"$a$b".toInt }
    }
    .toList
    .sum

  source.close()
  println(res)

case class Joltage(digits: Vector[Int]) {
  def updateWith(newDigit: Int): Joltage = {
    val (left, right) = digits.zip(digits.drop(1))
      .span { case (a, b) => a >= b }
    if (right.nonEmpty) {
      Joltage(left.map(_._1) ++ right.map(_._2) :+ newDigit)
    } else if (digits.last < newDigit) {
      Joltage(digits.updated(digits.size - 1, newDigit))
    } else {
      this
    }
  }
}

def part2(): Unit =
  val source = io.Source.fromResource("day3.txt")
  val res = source
    .getLines()
    .to(LazyList)
    .map { line =>
      val digits = line.toVector.map(_.toString.toInt)
      digits.drop(12).foldLeft(Joltage(digits.take(12))) { case (j, e) =>
        j.updateWith(e)
      }.pipe { j => j.digits.map(_.toString).mkString.toLong }
    }
    .toList
    .sum

  source.close()
  println(res)


@main
def main(): Unit =
  part2()