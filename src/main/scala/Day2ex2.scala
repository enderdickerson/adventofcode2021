import scala.io.Source

object Day2ex2 extends App {
  def navigate(chart: Seq[String]): Int = {
    val (hor, depth, _) = chart.map(s => s.split(" +").toSeq).foldLeft((0,0,0))((acc, cur) => {
      cur match {
        case Seq("forward", n) => (acc._1 + n.toInt, acc._2 + (n.toInt * acc._3), acc._3)
        case Seq("down", n) => (acc._1, acc._2, acc._3 + n.toInt)
        case Seq("up", n) => (acc._1, acc._2, acc._3 - n.toInt)
        case _ => (acc._1, acc._2, acc._3)
      }
    })
    hor * depth
  }

//  println(navigate(Source.fromFile("src/main/scala/day2inputtest.txt").getLines.toSeq))
  println(navigate(Source.fromFile("src/main/scala/day2input.txt").getLines.toSeq))
}
