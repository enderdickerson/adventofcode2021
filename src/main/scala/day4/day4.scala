package day4

import scala.io.Source

object day4 extends App {
  // parse in the bingo calls
  // parse the boards as single lines numbers in an ordered sequence
  // check positions for wins

  // check positions
  // vertical win
  // 0, 5, 10, 15, 20
  // 1, 6, 11, 16, 21
  // check(start index and i + 5 4 times)
  // compare against input list

  val parseCalls = (input: Seq[String]) =>
    input(0).split(",").toList

  val getBoards = (input: Seq[String]) => {
    input.foldLeft((Seq(), 0))((boards, row) => {
      if (row.isEmpty) (boards._1, boards._2 + 1)
      else {
        boards._1(boards._2)
      }
    })

  }

  val lines = Source.fromFile("src/main/scala/day4/day4test.txt").getLines.toSeq

  println(parseCalls(lines))
  println(getBoards(lines.drop(1)))
}
