package day8

import scala.annotation.tailrec
import scala.util.chaining.*

case class Coord(x: Long, y: Long, z: Long)

def distance(a: Coord, b: Coord): Double =
  math.sqrt(
    math.pow(a.x - b.x, 2) +
      math.pow(a.y - b.y, 2) +
      math.pow(a.z - b.z, 2)
  )

case class Graph(
  vertices: Set[Coord]
) {
  def addEdge(edge: (Coord, Coord)): Graph =
    copy(
      vertices + edge._1 + edge._2
    )

  def contains(edge: (Coord, Coord)): Boolean =
    vertices.contains(edge._1) || vertices.contains(edge._2)

  def merge(g: Graph): Graph =
    copy(vertices ++ g.vertices)
}

object Graph {
  def from(e: (Coord, Coord)): Graph =
    Graph(Set(e._1, e._2))
}

@tailrec
def findFirstCompleteEdge(
  graphs: List[Graph],
  previousEdge: (Coord, Coord),
  edges: List[(Coord, Coord)],
  allVertices: Set[Coord],
  minConnections: Option[Int],
): (List[Graph], (Coord, Coord)) = {
  if minConnections.contains(0) then
    (graphs, previousEdge)
  else if graphs.size > 1 || graphs.head.vertices != allVertices then
    val newEdge = edges.head
    val remainingEdges = edges.tail
    val (included, rest) = graphs.partition(_.contains(newEdge))
    val newGraphs = included.foldLeft(Graph.from(newEdge))(_ `merge` _) :: rest
    findFirstCompleteEdge(
      newGraphs,
      newEdge,
      remainingEdges,
      allVertices,
      minConnections.map(_ - 1),
    )
  else
    (graphs, previousEdge)
}

def solve(
  file: String,
  minConnections: Option[Int],
): Unit =
  val source = io.Source.fromResource(file)
  val junctionBoxes =
    source
      .getLines()
      .to(LazyList)
      .takeWhile(_.nonEmpty)
      .map { line =>
        val split = line.split(",")
        Coord(split(0).toLong, split(1).toLong, split(2).toLong)
      }
      .toList

  val edges = junctionBoxes
    .combinations(2)
    .to(LazyList)
    .map(_.toList)
    .map(l => l.head -> l(1))
    .sortBy(distance)
    .toList

  val (graphs, lastAddedEdge) = findFirstCompleteEdge(
    graphs = List(Graph.from(edges.head)),
    previousEdge = edges.head,
    edges = edges.tail,
    allVertices = junctionBoxes.toSet,
    minConnections = minConnections,
  )

  minConnections match {
    case Some(_) =>
      // Part 1
      graphs
        .map(_.vertices.size)
        .sorted(using Ordering[Int].reverse)
        .take(3)
        .product
        .tap(println)
    case None =>
      // Part 2
      lastAddedEdge match {
        case (a, b) =>
          println(a.x * b.x)
      }
  }

@main
def main(): Unit = {
  // PART1
  solve(
    "day8_2.txt",
    Some(1000),
  )

  // PART 2
  solve(
    "day8_2.txt",
    None,
  )
}