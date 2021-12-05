package day5

import scala.io.Source

object day5 extends App {
  case class Coord(x: Int, y: Int)
  case class Line(start: Coord, end: Coord) {
    def vert(): Boolean = start.x == end.x
    def hor(): Boolean = start.y == end.y
  }

  val parseLines = (input: Seq[String]) => {
    val cmdParser = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    input.map(x => {
      x match {
        case cmdParser(x1, y1, x2, y2) => Line(Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))
      }
    }).filter(x => x.vert() || x.hor())
  }

  val findBigVents = (input: Seq[String]) => {
    parseLines(input)
  }

  println(findBigVents(Source.fromFile("src/main/scala/day5/day5test.txt").getLines.toSeq))
}
