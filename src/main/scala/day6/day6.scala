package day6

import scala.io.Source

object day6 extends App {
  val growLanternFish = (input: String, days: Int) => {
    List.range(0, days)
      .foldLeft(input.split(",").map(_.toInt).groupBy(x => x).view.mapValues(x => BigInt(x.size)).toList)((acc, curr) => {
        acc.flatMap((x, y) => {
          x match {
            case 0 => List((8, y), (6, y))
            case n => List((n - 1, y))
          }
        }).groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum)).toList
      }).map(_._2).sum
  }

  println(growLanternFish(Source.fromFile("src/main/scala/day6/day6.txt").getLines.toList(0), 256))
}
