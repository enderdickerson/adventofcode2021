package day8

import scala.io.Source

object day8 extends App {
  val getDigitMap = (input: List[String]) => {
    val baseNumbers = input.map(x => {
      (x.length match {
        case 3 => "7"
        case 4 => "4"
        case 2 => "1"
        case 7 => "8"
        case _ => ""
      }, x.sorted)
    }).filter(_._1 != "").toMap

    val fourOneDiff = baseNumbers("4").filterNot(x => baseNumbers("1").contains(x))
    val eightFourDiff = baseNumbers("8").filterNot(x => baseNumbers("4").contains(x))

    input.filter(x => baseNumbers.values.filter(y => y == x).size == 0)
      .map(x => {
        x match {
          case n if n.size == 6 && fourOneDiff.filterNot(y => n.contains(y)).size == 1 => ("0", n)
          case n if n.size == 6 && baseNumbers("7").filterNot(y => n.contains(y)).size == 1 => ("6", n)
          case n if n.size == 6 && baseNumbers("7").filterNot(y => n.contains(y)).size == 0 => ("9", n)
          case n if n.size == 5 && eightFourDiff.filterNot(y => n.contains(y)).size == 0 => ("2", n)
          case n if n.size == 5 && baseNumbers("1").filterNot(y => n.contains(y)).size == 0 => ("3", n)
          case n if n.size == 5 && baseNumbers("1").filterNot(y => n.contains(y)).size == 1 => ("5", n)
          case m => ("unknown", m)
        }
      }).toMap.concat(baseNumbers).map((x, y) => (y, x)).toMap
  }

  val solveDisplay = (input: List[String], mapping: Map[String, String]) => {
    input.map(mapping(_)).mkString("").toInt
  }

  val solveOne = (segments: Array[Array[String]]) => {
    solveDisplay(segments(1).toList, getDigitMap(segments(0).sortBy(_.size).toList))
  }

  val calculate = (input: List[String]) => {
    input.map(x => solveOne(x.split('|').map(_.trim().split(' ').map(_.sorted)))).sum
  }

  println(calculate(Source.fromFile("src/main/scala/day8/day8.txt").getLines.toList))
}
