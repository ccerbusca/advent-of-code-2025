package day1

case class Dial(current: Int) {
  def turn(n: Int, direction: Char): (Dial, Int) =
    val sign = if direction == 'L' then -1 else 1
    val rotationCount = n / 100
    val left = n % 100
    val res = left * sign + current
    val _new =
      if (res < 0) 100 + res
      else if (res >= 100) res - 100
      else res
    val newRotations =
      if (_new != res || _new == 0) && current != 0 then 1 else 0
    (Dial(_new), rotationCount + newRotations)
}

def part1(): Unit =
  val source = io.Source.fromResource("day1.txt")
  val res = source
    .getLines()
    .to(LazyList)
    .foldLeft((Dial(50), 0)) { case ((dial, count), line) =>
      val (newDial, _) = dial.turn(line.drop(1).toInt, line.head)
      (newDial, count + (if newDial.current == 0 then 1 else 0))
    }
    ._2
  source.close()
  println(res)

def part2(): Unit =
  val source = io.Source.fromResource("day1.txt")
  val res = source
    .getLines()
    .to(LazyList)
    .foldLeft((Dial(50), 0)) { case ((dial, count), line) =>
      val (newDial, zeroCount) = dial.turn(line.drop(1).toInt, line.head)
      println((newDial, zeroCount))
      (newDial, count + zeroCount)
    }
    ._2
  source.close()
  println(res)


@main
def main(): Unit =
  part2()