import scala.io.Source

object Day1ex1 extends App {
  val depthChecker = (input: Seq[String]) =>
    input.zipWithIndex.drop(1).foldLeft(0) {
      case (acc, (curr, index)) => {
        if (curr.toInt > input(index - 1).toInt) acc + 1 else acc
      }
    }

//  println(depthChecker(Source.fromFile("src/main/scala/day1inputtest.txt").getLines.toSeq))
  println(depthChecker(Source.fromFile("src/main/scala/day1input.txt").getLines.toSeq))
}
