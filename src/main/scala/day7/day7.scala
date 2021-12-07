package day7

import scala.io.Source

object day7 extends App {
  val calculateConvergence = (input: List[String]) => {
    val crabs = input.map(_.toInt).sorted
    (crabs(0) to crabs.last).foldLeft(Map[Int,Int]())((acc, curr) => {
      acc.updated(curr, crabs.foldLeft(0)((fuel, cb) => {
        fuel + (curr - cb).abs
      }))
    }).minBy(_._2)
  }

  println(calculateConvergence(Source.fromFile("src/main/scala/day7/day7.txt").getLines.toList(0).split(",").toList))
}
