package day10

import scala.io.Source

object day10 extends App {
  val matchesOpener = (opener: Char, closer: Char) => {
    val expectedCloser = opener match {
      case '[' => ']'
      case '(' => ')'
      case '{' => '}'
      case '<' => '>'
    }
    expectedCloser == closer
  }

  val checkLine = (input: String) => {
    input.toList.foldLeft((List[Char](), ""))((acc, curr) => {
      if (!acc._2.isEmpty) acc
      else if ("{<([".toList.contains(curr)) {
//        println(s"found opening: $curr")
        (acc._1 :+ curr, "")
      }
      else if (!matchesOpener(acc._1.last, curr)) {
//        println(s"found error $curr - expected ${acc._1.last}")
        (acc._1, curr.toString)
      }
      else {
//        println(s"found matching closer $curr for ${acc._1.last}")
        (acc._1.dropRight(1), "")
      }
    })._2
  }

  val calcLineScore = (input: List[String]) => {
    input.map(x => checkLine(x)).filter(x => !x.isEmpty).map(x => {
      x match {
        case ")" => 3
        case "]" => 57
        case "}" => 1197
        case ">" => 25137
      }
    }).sum
  }

  println(calcLineScore(Source.fromFile("src/main/scala/day10/day10.txt").getLines.toList))
}
