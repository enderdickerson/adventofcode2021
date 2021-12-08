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
  // 4 (bcdf) - 1 (cf) = bc

  // to calculate 'a' wire
    // find the different letter between the size 2 sequence and the size 3 sequence


  val calculate = (input: List[String]) =>
    input
      .map(x => x.split('|')(1).trim().split(' ').toList
        .map(x => {
          x.length match {
            case 3 => 7
            case 4 => 4
            case 2 => 1
            case 7 => 8
            case _ => 0
          }
        }))
      .map(x => x.filter(_ != 0).size)
      .sum

  println(calculate(Source.fromFile("src/main/scala/day8/day8.txt").getLines.toList))
}
