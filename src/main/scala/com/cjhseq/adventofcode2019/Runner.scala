package com.cjhseq.adventofcode2019

import scala.io.Source
// import scala.util.Try

object Runner extends App {
  val day: Int = args.headOption.map(_.toInt).getOrElse(1)
  val resource_string: String = s"${"%02d".format(day)}.txt"
  val inputs: List[String] = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(resource_string)).getLines().toList

  val res = day match {
    case 1 => Day01.solve(inputs)
    case 2 => Day02.solve(inputs)
    case 3 => Day03.solve(inputs)
    case 4 => Day04.solve(inputs)
    case 5 => Day05.solve(inputs)
    case 6 => Day06.solve(inputs)
  }

  println(res)
}
