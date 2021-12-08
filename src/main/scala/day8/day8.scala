package day8

import scala.io.Source

object day8 extends App {
  // size of 6 not 6 or 9 => 0 abcefg
  // size of 2  => 1 cf
  // size of 5, take 8 - 4 and length of 2 = 3 => 2 acdeg
    // 2 doesn't have one segment from 1
  // size of 5 and has both from 1 => 3 (acdfg)

  // size of 4  => 4 bcdf
  // size of 5, take 8 - 4 and length of 5 = 4  => 5 abdfg
  // size of 6 take 4 - 1, 6 is missing a letter => 6 abdefg
    // 6 doesn't have one segment from 1
  // size of 3  => 7 acf
  // size of 7  => 8 abcdefg
  // size of 6 take 8 - 4, 9 is missing a letter => 9 abcdfg

  // 9,6,0 have 6 segments
    // abcdfg = 9
    // abdefg = 6
    // abcefg = 0
  // 5,2,3 have 5 segments
    // abdfg = 5
    // acdeg = 2

  // 8 (abcdefg)- 4 (bcdf) = (aeg) - a = eg
  // 4 (bcdf) - 1 (cf) = bd

  // to calculate 'a' wire
    // find the different letter between the size 2 sequence and the size 3 sequence

  val convert = (input: List[String]) => {
    val baseNumbers = input.map(x => {
      (x.length match {
        case 3 => "7"
        case 4 => "4"
        case 2 => "1"
        case 7 => "8"
        case _ => ""
      }, x.sorted)}).filter(_._1 != "").toMap

    val others = input.filter(x => baseNumbers.values.filter(y => y == x).size == 0)

    val fourOneDiff = baseNumbers("4").filter(x => baseNumbers("1").contains(x))

//    val mapped = others.map(x => {
//      x match {
//        case n if n.size == 6 &&
//      }
//    })

    println(s"others: $others")
    println(s"fourOneDiff: $fourOneDiff")

    baseNumbers
  }

  val solveOne = (input: String) => {
    val allDigits = input.split('|')(0).trim().split(' ').sortBy(_.size).map(_.sorted).toList
    val converted = convert(allDigits)
    converted
  }

  val calculate = (input: List[String]) => {
    solveOne(input(0))
//    val endValues = input.map(x => x.split('|')(1).trim().split(' ').toList)
//    val startValues = input.map(x => x.)
//    endValues

//      .map(x => {
//        (x.length match {
//          case 3 => "7"
//          case 4 => "4"
//          case 2 => "1"
//          case 7 => "8"
//          case _ => ""
//        }, x.sorted)
//      }).filter(x => x._1 != ""))
  }
  //      .map(x => x.filter(_ != 0))

  println(calculate(Source.fromFile("src/main/scala/day8/day8test.txt").getLines.toList))
}
