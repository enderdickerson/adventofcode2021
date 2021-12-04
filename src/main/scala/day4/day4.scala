package day4

import scala.io.Source

object day4 extends App {
  val parseCalls = (input: Seq[String]) =>
    input(0).split(",").toList

  val addIfEmpty = (input: Seq[Seq[String]], index: Int) =>
    if (!(input.size >= index + 1)) input :+ Seq[String]() else input

  val getBoards = (input: Seq[String]) => {
    input.foldLeft((Seq[Seq[String]](), 0))((acc, row) => {
      val (boards, index) = acc
      if (row.isEmpty) (boards, index + 1)
      else {
        val atIndex = addIfEmpty(boards, index)
        val rowToAdd = row.trim().split(" +").toSeq
        (atIndex.updated(index, atIndex(index) ++ rowToAdd), index)
      }
    })._1
  }

  val checkVerticalWin = (board: Seq[String], played: Seq[String]) => {
    val remainingPieces = board.grouped(5).toList.transpose.map(_.filterNot(played.toSet))
    if (remainingPieces.filter(_.isEmpty).size > 0) {
      remainingPieces.filter(!_.isEmpty).map(x => x.map(_.toInt).sum).sum
    } else 0
  }

  val checkHorizontalWin = (board: Seq[String], played: Seq[String]) => {
    val remainingPieces = board.grouped(5).toList.map(_.filterNot(played.toSet))
    if (remainingPieces.filter(_.isEmpty).size > 0) {
      remainingPieces.filter(!_.isEmpty).map(x => x.map(_.toInt).sum).sum
    } else 0
  }

  val calculateScore = (board: Seq[String], played: Seq[String]) => {
    List(checkVerticalWin(board, played), checkHorizontalWin(board, played)).sum * played.last.toInt
  }

  val filterBoards = (boards: Seq[Seq[String]], played: Seq[String]) => {
    val wonVertically = boards.filter(board => checkVerticalWin(board, played) > 0)
    val wonHorizontally = boards.filter(board => checkHorizontalWin(board, played) > 0)
    if (wonVertically.size > 0) wonVertically
    else if (wonHorizontally.size > 0) wonHorizontally
    else boards
  }

  val play = (plays: List[String], boards: Seq[Seq[String]]) => {
    val (winningBoard, played) = plays.foldLeft((boards,Seq[String]()))((acc, curr) => {
      val (boardSeq, played) = acc
      if (boardSeq.size <= 1) acc
      else {
        val withPlay = played :+ curr
        (filterBoards(boardSeq, withPlay), withPlay)
      }
    })
    (winningBoard(0), played)
  }

  val lines = Source.fromFile("src/main/scala/day4/day4.txt").getLines.toSeq
  val (winningBoard,played) = play(parseCalls(lines), getBoards(lines.drop(2)))
  val score = calculateScore(winningBoard, played)

  println(s"Winning round - score of: $score")
}
