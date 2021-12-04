package day4

import scala.io.Source

object day4 extends App {
  val parseCalls = (input: Seq[String]) => input(0).split(",").toList

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

  val calculateScore = (board: Seq[String], played: Seq[String]) =>
    (checkVerticalWin(board, played) max checkHorizontalWin(board, played)) * played.last.toInt

  val filterBoards = (boards: Seq[Seq[String]], played: Seq[String]) => {
    val wonVertically = boards.filter(board => checkVerticalWin(board, played) > 0)
    val wonHorizontally = boards.filter(board => checkHorizontalWin(board, played) > 0)
    if (wonVertically.size > 0) wonVertically
    else if (wonHorizontally.size > 0) wonHorizontally
    else boards
  }

  val playToWin = (plays: List[String], boards: Seq[Seq[String]]) => {
    val (winningBoard, played) = plays.foldLeft((boards,Seq[String]()))((acc, curr) => {
      val (boardSeq, played) = acc
      if (boardSeq.size <= 1) acc
      else {
        val withPlay = played :+ curr
        (filterBoards(boardSeq, withPlay), withPlay)
      }
    })
    calculateScore(winningBoard(0), played)
  }

  val filterWinningBoards = (boards: Seq[Seq[String]], played: Seq[String]) =>
    boards.filter(board => checkVerticalWin(board, played) > 0 || checkHorizontalWin(board, played) > 0)

  val filterActiveBoards = (boards: Seq[Seq[String]], played: Seq[String]) =>
    boards.filter(board => checkVerticalWin(board, played) == 0 && checkHorizontalWin(board, played) == 0)

  val playToLose = (plays: List[String], boards: Seq[Seq[String]]) => {
    val (_, winners, played) = plays.foldLeft((boards, Seq[Seq[String]](), Seq[String]()))((acc, curr) => {
      val (inPlay, winners, played) = acc
      if (winners.size == boards.size) acc
      else {
        val withPlay = played :+ curr
        (filterActiveBoards(inPlay, withPlay), winners ++ filterWinningBoards(inPlay, withPlay), withPlay)
      }
    })
    calculateScore(winners.last, played)
  }

  val lines = Source.fromFile("src/main/scala/day4/day4.txt").getLines.toSeq
//  println(playToWin(parseCalls(lines), getBoards(lines.drop(2))))
  println(playToLose(parseCalls(lines), getBoards(lines.drop(2))))
}
