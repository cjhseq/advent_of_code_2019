package com.cjhseq.adventofcode2019

import scala.io.Source
// import scala.util.Try

object Runner extends App {
  val day: Int = args.headOption.map(_.toInt).getOrElse(1)
  val resource_string: String = s"${"%02d".format(day)}.txt"
  val inputs: List[String] = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(resource_string)).getLines().toList

  val res = day match {
    case 1 => Day01.solve(inputs)
  }

  println(res)
}
