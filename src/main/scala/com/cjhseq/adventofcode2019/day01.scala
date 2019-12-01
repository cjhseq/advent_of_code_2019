package com.cjhseq.adventofcode2019

import scala.math.floor

object Day01{

  def fuel_calc(mass: Int): Int = {
    floor(mass / 3).toInt - 2
  }

  @scala.annotation.tailrec
  def fuel_calc_more_fuel(mass: Int, current_fuel: Int = 0): Int = {
    val fuel = fuel_calc(mass)
    fuel match {
      case x if x <= 0 => current_fuel
      case x if x > 0 => fuel_calc_more_fuel(fuel, current_fuel + fuel)
    }
  }

  def part_1(inputs: List[String]): Option[Int] = {
    if (inputs.length == 0){
      return None
    }
    val total: Int = inputs.map(x => fuel_calc(x.toInt)).sum
    return Some(total)
  }

  def part_2(inputs: List[String]): Option[Int] = {
    if (inputs.length == 0){
      return None
    }
    val total: Int = inputs.map(x => fuel_calc_more_fuel(x.toInt, 0)).sum
    return Some(total)
  }

  def solve(inputs: List[String]): (Option[Int], Option[Int]) = {
    val res1 = part_1(inputs)
    val res2 = part_2(inputs)
    return (res1, res2)
  }
}
