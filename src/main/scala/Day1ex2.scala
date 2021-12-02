import scala.io.Source

object Day1ex2 extends App {
  val sumIncreases = (input: Seq[Int]) =>
    input.zipWithIndex.drop(1).foldLeft(0) {
      case (acc, (curr, index)) => {
        if (curr> input(index - 1)) acc + 1 else acc
      }
    }

  val asThreeWindow = (input: Seq[String]) =>
    input.zipWithIndex.take(input.size - 2).map((x, y) => x.toInt + input(y + 1).toInt + input(y + 2).toInt)

  println(sumIncreases(asThreeWindow(Source.fromFile("src/main/scala/day1input.txt").getLines.toSeq)))
}

