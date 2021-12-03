import scala.io.Source

object Day3 extends App {
  val findPwrConsumption = (input: Seq[String]) =>
    input.map(_.toList.map(_.toString.toInt)).transpose
      .map(_.groupBy(y => y).view.mapValues(_.size).toMap)
      .map(x => List(x.maxBy(_._2)._1, x.minBy(_._2)._1))
      .transpose
      .map(x => Integer.parseInt(x.mkString, 2))
      .product

//  println(findPwrConsumption(Source.fromFile("src/main/scala/day3inputtest.txt").getLines.toSeq))
  println(findPwrConsumption(Source.fromFile("src/main/scala/day3input.txt").getLines.toSeq))
}
