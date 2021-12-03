import scala.io.Source

object day1 extends App {
  val depthChecker = (input: Seq[String]) =>
    input.zipWithIndex.drop(1).foldLeft(0) {
      case (acc, (curr, index)) => {
        if (curr.toInt > input(index - 1).toInt) acc + 1 else acc
      }
    }

  val sumIncreases = (input: Seq[Int]) =>
    input.zipWithIndex.drop(1).foldLeft(0) {
      case (acc, (curr, index)) => {
        if (curr> input(index - 1)) acc + 1 else acc
      }
    }

  val asThreeWindow = (input: Seq[String]) =>
    input.zipWithIndex.take(input.size - 2).map((x, y) => x.toInt + input(y + 1).toInt + input(y + 2).toInt)

//  println(depthChecker(Source.fromFile("src/main/scala/day1inputtest.txt").getLines.toSeq))
  println(depthChecker(Source.fromFile("src/main/scala/day1/day1input.txt").getLines.toSeq))
  println(sumIncreases(asThreeWindow(Source.fromFile("src/main/scala/day1/day1input.txt").getLines.toSeq)))
}
