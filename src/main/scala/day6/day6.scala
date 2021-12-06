package day6

import scala.io.Source

object day6 extends App {
  val growLanternfish = (input: List[String], days: Int) => {
    List.range(0, days)
      .foldLeft(input(0).split(",").map(_.toInt))((acc, _) =>
        acc.flatMap(fish => if (fish == 0) List(6, 8) else List(fish - 1))).size
  }

  println(growLanternfish(Source.fromFile("src/main/scala/day6/day6.txt").getLines.toList, 80))
}
