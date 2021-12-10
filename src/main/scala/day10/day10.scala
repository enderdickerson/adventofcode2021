package day10

import scala.io.Source

object day10 extends App {
  val getCloser = (opener: Char) => {
    opener match {
      case '[' => ']'
      case '(' => ')'
      case '{' => '}'
      case '<' => '>'
    }
  }

  val checkLine = (input: String) => {
    input.toList.foldLeft((List[Char](), ""))((acc, curr) => {
      if (!acc._2.isEmpty) acc
      else if ("{<([".toList.contains(curr)) (acc._1 :+ curr, "")
      else if (getCloser(acc._1.last) != curr) (acc._1, curr.toString)
      else (acc._1.dropRight(1), "")
    })
  }

  val calcLineScore = (input: List[String]) => {
    input.map(x => checkLine(x)._2).filter(x => !x.isEmpty).map(x => {
      x match {
        case ")" => 3
        case "]" => 57
        case "}" => 1197
        case ">" => 25137
      }
    }).sum
  }

  val autocomplete = (input: List[Char]) => {
    input.reverse.foldLeft(List[Char]())((acc, curr) => {
      acc :+ getCloser(curr)
    })
  }

  val score = (input: List[Char]) => {
    input.map(x => {
      x match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case '>' => 4
      }
    }).foldLeft(BigInt(0))((acc, curr) => {
      acc * 5 + curr
    })
  }

  val calcAutocomplete = (input: List[String]) => {
    val scores = input.map(x => checkLine(x)).filter((x,y) => y.isEmpty).map((x,y) => autocomplete(x)).map(score).sorted
    scores((scores.size - 1) / 2)
  }

//  println(calcLineScore(Source.fromFile("src/main/scala/day10/day10.txt").getLines.toList))
  println(calcAutocomplete(Source.fromFile("src/main/scala/day10/day10.txt").getLines.toList))
}
