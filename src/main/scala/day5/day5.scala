package day5

import scala.io.Source

object day5 extends App {
  case class Coord(x: Int, y: Int)
  case class Line(start: Coord, end: Coord) {
    def vert(): Boolean = start.x == end.x
    def hor(): Boolean = start.y == end.y
    def expand(): List[Coord] = {
      if (vert()) {
        val range = if (start.y < end.y) start.y to end.y else end.y to start.y
        range.foldLeft(List[Coord]())((acc, curr) => {
          acc :+ Coord(start.x, curr)
        })
      }
      else {
        val range = if (start.x < end.x) start.x to end.x else end.x to start.x
        range.foldLeft(List[Coord]())((acc, curr) => {
          acc :+ Coord(curr, start.y)
        })
      }
    }
  }

  val cmdParser = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r

  val findBigVents = (input: Seq[String]) => {
    input.map(x => {
        x match {
          case cmdParser(x1, y1, x2, y2) => Line(Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))
        }
      }).filter(x => x.vert() || x.hor()).flatMap(x => x.expand()).groupBy(x => x).mapValues(_.size)
      .toMap.filter(x => x._2 > 1).size
  }

  println(findBigVents(Source.fromFile("src/main/scala/day5/day5.txt").getLines.toSeq))
}
