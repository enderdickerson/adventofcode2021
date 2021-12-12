package day11

import scala.io.Source

object day11 extends App {
  case class Dumbo(val power: Int, val flashed: Boolean, val xPos: Int, val yPos: Int) {
    def reset(): Dumbo = {
      this.copy(flashed = false)
    }

    def increase(): Dumbo = {
      this.copy(power = power + 1)
    }

    def stepFlash(): Dumbo = {
      this.copy(power = 0, flashed = true)
    }

    def flashAvailable: Boolean = !flashed && power > 8
    def neighbors: List[(Int,Int)] = {
      List((-1,0),
      (1,0),
      (0,-1),
      (0,1),
      (-1,-1),
      (-1,1),
      (1,-1),
      (1,1))
    }
  }
  case class DumboSchool(val school: List[List[Dumbo]]) {
    def update(dumbo: Dumbo): DumboSchool = {
      DumboSchool(school.updated(dumbo.yPos, school(dumbo.yPos).updated(dumbo.xPos, dumbo)))
    }

    def belongsToSchool(dumbo: Dumbo, modifier: (Int,Int)): Boolean =
      school.size > (dumbo.yPos + modifier._1) && (dumbo.yPos + modifier._1) > -1
        && (school(dumbo.yPos + modifier._1).size > (dumbo.xPos + modifier._2) && (dumbo.xPos + modifier._2) > -1)
  }

  def flash(dumbos: DumboSchool, dumbo: Dumbo): DumboSchool = {
    if (dumbo.flashAvailable) {
      dumbo.neighbors
        .filter(x => dumbos.belongsToSchool(dumbo, x))
        .foldLeft(dumbos.update(dumbo.stepFlash()))((acc, curr) => {
        flash(acc, acc.school(dumbo.yPos + curr._1)(dumbo.xPos + curr._2))
      })
    }
    else if (dumbo.flashed) dumbos
    else dumbos.update(dumbo.increase())
  }

  val step = (input: List[List[Dumbo]]) => {
    val increased = DumboSchool(input.map(x => x.map(y => y.increase())))
    val afterFlashing = increased.school.flatten.filter(x => x.power > 9).foldLeft(increased)((acc, curr) => {
      flash(acc, acc.school(curr.yPos)(curr.xPos))
    })

    val flashed = afterFlashing.school.flatten.map(x => x.flashed).filter(x => x).size

    (afterFlashing.school.map(x => x.map(y => y.reset())), flashed)
  }

  val calc = (input: List[String], end: Int) => {
    val dumbos = input.map(x => x.map(y => y.toString.toInt).toList).toList
      .foldLeft(List[List[Dumbo]]())((rows, rowItem) => {
        rows :+ rowItem.foldLeft(List[Dumbo]())((cols, colItem) => {
          cols :+ Dumbo(colItem, false, cols.size, rows.size)
        })
      })

    val res = (1 to end).foldLeft((dumbos, 0))((acc, curr) => {
      val stepped = step(acc._1)
      if (stepped._2 == acc._1.flatten.size) {
        println(s"flash event: $curr")
      }
      (stepped._1, acc._2 + stepped._2)
    })

    println(s"total flashes: ${res._2}")
  }

  calc(Source.fromFile("src/main/scala/day11/day11.txt").getLines.toList, 500)
}
