package day9

import scala.io.Source

object day9 extends App {
  case class LowPoint(row: Int, pos: Int, height: Int) {
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

  val findLowPoints = (input: List[List[Int]]) => {
    input.foldLeft((List[List[LowPoint]](),0))((acc, curr) => {
      val rows = curr.foldLeft((List[LowPoint](),0))((colAcc, colCurr) => {
        if (isLowPoint(input, acc._2, colAcc._2, colCurr)) {
          (colAcc._1 :+ LowPoint(acc._2, colAcc._2, colCurr), colAcc._2 + 1)
        }
        else (colAcc._1, colAcc._2 + 1)
      })
      (acc._1 :+ rows._1, acc._2 + 1)
    })._1.filter(_.size > 0).flatten.map(x => x.risk).sum
  }

  println(findLowPoints(Source.fromFile("src/main/scala/day9/day9.txt").getLines.toList.map(e => e.map(x => x.toString.toInt).toList)))
}
