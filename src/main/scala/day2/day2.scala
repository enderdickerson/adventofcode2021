import scala.io.Source

object day2 extends App {
  def simpleNavigate(chart: Seq[String]): Int = {
    val (hor, depth) = chart.map(s => s.split(" +").toSeq).foldLeft((0, 0))((acc, cur) => {
      cur match {
        case Seq("forward", n) => (acc._1 + n.toInt, acc._2)
        case Seq("down", n) => (acc._1, acc._2 + n.toInt)
        case Seq("up", n) => (acc._1, acc._2 - n.toInt)
        case _ => (acc._1, acc._2)
      }
    })
    hor * depth
  }

  def aimedNavigate(chart: Seq[String]): Int = {
    val (hor, depth, _) = chart.map(s => s.split(" +").toSeq).foldLeft((0, 0, 0))((acc, cur) => {
      cur match {
        case Seq("forward", n) => (acc._1 + n.toInt, acc._2 + (n.toInt * acc._3), acc._3)
        case Seq("down", n) => (acc._1, acc._2, acc._3 + n.toInt)
        case Seq("up", n) => (acc._1, acc._2, acc._3 - n.toInt)
        case _ => (acc._1, acc._2, acc._3)
      }
    })
    hor * depth
  }

  println(simpleNavigate(Source.fromFile("src/main/scala/day2/day2input.txt").getLines.toSeq))
  println(aimedNavigate(Source.fromFile("src/main/scala/day2/day2input.txt").getLines.toSeq))
}
