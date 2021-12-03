import scala.io.Source

object Day3pt1 extends App {
  val findPwrConsumption: Function1[Seq[String], Int] = (input: Seq[String]) => {
    val asGroups = input.map(_.toList.map(x => x.toString.toInt))
    val transposed = asGroups.transpose
    println(asGroups)
    println(transposed)
    val withCount = transposed.map(x => x.groupBy(y => y).view.mapValues(z => z.size).toMap)
    println(withCount)
    val gamma = withCount.map(x => x.maxBy(_._2)._1)
    val epsilon = withCount.map(x => x.minBy(_._2)._1)

    val gammaVal = Integer.parseInt(gamma.mkString, 2)
    val epsilonVal = Integer.parseInt(epsilon.mkString, 2)
    println(gammaVal)
    println(epsilonVal)
    gammaVal * epsilonVal
  }

  println(findPwrConsumption(Source.fromFile("src/main/scala/day3inputtest.txt").getLines.toSeq))
}
