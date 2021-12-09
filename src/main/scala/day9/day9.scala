package day9

import scala.io.Source

object day9 extends App {
  case class Point(row: Int, pos: Int, height: Int) {
    def risk: Int = height + 1
  }

  val isLowPoint = (rows: List[List[Int]], rowIndex: Int, position: Int, height: Int) => {
    val hasLeft = position - 1 > -1
    val hasRight = position + 1 < rows(rowIndex).size
    val hasUp = rowIndex - 1 > -1
    val hasDown = rowIndex + 1 < rows.size

    val leftCheck = !hasLeft || height < rows(rowIndex)(position - 1)
    val rightCheck = !hasRight || height < rows(rowIndex)(position + 1)
    val upCheck = !hasUp || height < rows(rowIndex - 1)(position)
    val downCheck = !hasDown || height < rows(rowIndex + 1)(position)

    leftCheck && rightCheck && upCheck && downCheck
  }

  def findBasin(rows: List[List[Int]], point: Point, points: List[Point]): List[Point] = {
    if (points.contains(point) || point.height == 9) points
    else {
      val hasLeft = point.pos - 1 > -1
      val hasRight = point.pos + 1 < rows(point.row).size
      val hasUp = point.row - 1 > -1
      val hasDown = point.row + 1 < rows.size
      val addCurrent = points :+ point

      val left = if (hasLeft && rows(point.row)(point.pos - 1) >= point.height) {
        findBasin(rows, Point(point.row, point.pos - 1, rows(point.row)(point.pos - 1)), addCurrent)
      } else List[Point]()

      val right = if (hasRight && rows(point.row)(point.pos + 1) >= point.height) {
        findBasin(rows, Point(point.row, point.pos + 1, rows(point.row)(point.pos + 1)), addCurrent)
      } else List[Point]()

      val up = if (hasUp && rows(point.row - 1)(point.pos) >= point.height) {
        findBasin(rows, Point(point.row - 1, point.pos, rows(point.row - 1)(point.pos)), addCurrent)
      } else List[Point]()

      val down = if (hasDown && rows(point.row + 1)(point.pos) >= point.height) {
        findBasin(rows, Point(point.row + 1, point.pos, rows(point.row + 1)(point.pos)), addCurrent)
      } else List[Point]()

      addCurrent ++ left ++ right ++ up ++ down
    }
  }

  val findLowPoints = (input: List[List[Int]]) => {
    input.foldLeft((List[List[Point]](), 0))((acc, curr) => {
      val rows = curr.foldLeft((List[Point](), 0))((colAcc, colCurr) => {
        if (isLowPoint(input, acc._2, colAcc._2, colCurr)) {
          (colAcc._1 :+ Point(acc._2, colAcc._2, colCurr), colAcc._2 + 1)
        }
        else (colAcc._1, colAcc._2 + 1)
      })
      (acc._1 :+ rows._1, acc._2 + 1)
    })._1.filter(_.size > 0).flatten
  }

  val findLowPointRisk = (input: List[List[Int]]) => findLowPoints(input).map(_.risk).sum

  val findBasinRisk = (input: List[List[Int]]) =>
    findLowPoints(input).map(x => findBasin(input, x, List[Point]()).toSet.toList)
      .map(x => x.size).sorted.reverse.take(3).product

  //  println(findLowPointRisk(Source.fromFile("src/main/scala/day9/day9test.txt").getLines.toList.map(e => e.map(x => x.toString.toInt).toList)))
  println(findBasinRisk(Source.fromFile("src/main/scala/day9/day9.txt").getLines.toList.map(e => e.map(x => x.toString.toInt).toList)))
}
