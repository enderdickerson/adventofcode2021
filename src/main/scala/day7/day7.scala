package day7

import scala.io.Source

object day7 extends App {
  val moveCrabsConstant = (input: List[String]) => {
    val crabs = input.map(_.toInt).sorted
    (crabs(0) to crabs.last).foldLeft(Map[Int,Int]())((acc, curr) => {
      acc.updated(curr, crabs.foldLeft(0)((fuel, cb) => {
        fuel + (curr - cb).abs
      }))
    }).minBy(_._2)
  }

  val moveCrabsExpensive = (input: List[String]) => {
    val crabs = input.map(_.toInt).sorted
    (crabs(0) to crabs.last).foldLeft(Map[Int,Int]())((acc, curr) => {
      acc.updated(curr, crabs.foldLeft(0)((fuel, cb) => {
        fuel + (1 to (cb - curr).abs).foldLeft(0)(_ + _)
      }))
    }).minBy(_._2)
  }

  println(moveCrabsConstant(Source.fromFile("src/main/scala/day7/day7.txt").getLines.toList(0).split(",").toList))
  println(moveCrabsExpensive(Source.fromFile("src/main/scala/day7/day7.txt").getLines.toList(0).split(",").toList))
}
