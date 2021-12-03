import scala.io.Source

object day3 extends App {
  val findPwrConsumption = (input: Seq[String]) =>
    input.map(_.toList.map(_.toString.toInt)).transpose
      .map(_.groupBy(y => y).view.mapValues(_.size).toMap)
      .map(x => List(x.maxBy(_._2)._1, x.minBy(_._2)._1))
      .transpose
      .map(x => Integer.parseInt(x.mkString, 2))
      .product

  val findInReport = (parsed: Seq[List[Int]], transposed: Seq[Seq[Int]], f: (min: (Int, Int), max: (Int, Int)) => Int) => {
    transposed.foldLeft((parsed, 0))((acc, cur) => {
      val (holdouts, index) = acc
      if (holdouts.size == 1) acc
      else {
        val counts = holdouts.map(x => x(index)).groupBy(x => x).view.mapValues(_.size).toMap
        (holdouts.filter(x => x(index) == f(counts.minBy(_._2), counts.maxBy(_._2))), index + 1)
      }
    })._1(0)
  }

  val findLifeSupport = (input: Seq[String]) => {
    val parsed = input.map(_.toList.map(_.toString.toInt))
    val oxygenComparer = (min: (Int, Int), max: (Int, Int)) => if (max._2 == min._2) 1 else max._1
    val co2Comparer = (min: (Int, Int), max: (Int, Int)) => if (max._2 == min._2) 0 else min._1

    val oxyGenRating = findInReport(parsed, parsed.transpose, oxygenComparer)
    val co2ScruberRating = findInReport(parsed, parsed.transpose, co2Comparer)

    List(oxyGenRating, co2ScruberRating)
      .map(x => Integer.parseInt(x.mkString, 2))
      .product
  }

  //  println(findPwrConsumption(Source.fromFile("src/main/scala/day3/day3inputtest.txt").getLines.toSeq))
  //  println(findPwrConsumption(Source.fromFile("src/main/scala/day3/day3input.txt").getLines.toSeq))
  println(findLifeSupport(Source.fromFile("src/main/scala/day3/day3inputtest.txt").getLines.toSeq))
  println(findLifeSupport(Source.fromFile("src/main/scala/day3/day3input.txt").getLines.toSeq))
}
