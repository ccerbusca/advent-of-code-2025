package day6

import scala.util.chaining.*

def part1(): Unit =
  val source = io.Source.fromResource("day6.txt")
  val input = source
    .getLines()
    .to(Vector)
    .takeWhile(_.nonEmpty)
    .map(line => line.split("\\s+").toVector.filter(_.nonEmpty))
    .transpose
    .map { line =>
      val op = line.last
      line.dropRight(1).map(_.toLong).reduce { case (a, b) =>
        op match {
          case "+" => a + b
          case "*" => a * b
        }
      }
    }
    .sum
    .tap(println)

  source.close()
  
case class Column(
  operation: Char,
  size: Int,
)

def part2(): Unit =
  val source = io.Source.fromResource("day6.txt")
  val input = source
    .getLines()
    .to(Vector)
    .takeWhile(_.nonEmpty)

  val operations =
    "[*+]\\s+".r
      .findAllIn(input.last.appended(' '))
      .collect { case operationStr =>
        Column(
          operation = operationStr.head,
          size = operationStr.length - 1,
        )
      }
      .toVector

  input.dropRight(1)
    .map { line =>
      operations
        .foldLeft((line, Vector.empty[String])) {
          case ((remainingLine, acc), operation) =>
            (
              remainingLine.drop(operation.size + 1),
              acc appended remainingLine.take(operation.size)
            )
        }
        ._2
    }
    .transpose
    .zipWithIndex
    .map { case (line, index) =>
      line.transpose.map(_.mkString.trim.toLong)
        .reduce { case (a, b) =>
          operations(index).operation match {
            case '+' => a + b
            case '*' => a * b
          }
        }
    }
    .sum
    .tap(println)

  source.close()

@main
def main(): Unit =
  part2()