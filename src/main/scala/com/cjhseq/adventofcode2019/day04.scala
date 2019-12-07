package com.cjhseq.adventofcode2019



object Day04{

  def check_criteria(current_digit: Int, password: List[String], criteria_flag: Int = 0): Boolean ={
    val next :: rest = password
    val next_digit = next.toInt
    val flag = current_digit match {
      case x if x == next_digit => 1
      case x if x > next_digit => 2
      case x if x < next_digit => 0
    }
    val new_flag = criteria_flag | flag

    rest.length match {
      case 0 => new_flag == 1
      case _ => check_criteria(next_digit, rest, new_flag)
    }
  }

  def check_groups(current_digit: Int, password: List[String], shortest_run: Int = 6, current_run: Int = 1): Boolean = {
    val next :: rest = password
    val next_digit = next.toInt
    val run = current_digit match {
      case x if x == next_digit => current_run + 1
      case _ => 1
    }
    val new_short = if ((run == 1) && (current_run > 1) && (current_run < shortest_run)) {
      current_run
    } else if((rest.length == 0) && (run > 1) && (run < shortest_run)) {
      run
    } else {
        shortest_run
    }
    rest.length match {
      case 0 => new_short == 2
      case _ => check_groups(next_digit, rest, new_short, run)
    }

  }

  def check_criteria_wrapper(password: Int): Boolean = {
    val first = password.toString.take(1)
    val rest = password.toString.takeRight(5).split("").toList
    check_criteria(first.toInt, rest)
  }

  def check_criteria_wrapper_2(password: Int): Boolean = {
    val first = password.toString.take(1)
    val rest = password.toString.takeRight(5).split("").toList
    if (check_criteria(first.toInt, rest)) check_groups(first.toInt, rest) else false
  }

  def part_1(inputs: List[String]): Option[Int] = {
    val range = inputs.head.split("-")
    val matching = (for (i <- range(0).toInt to range(1).toInt) yield check_criteria_wrapper(i)).toList
    Some(matching.map(x=> if(x) 1 else 0).sum)
  }

  def part_2(inputs: List[String]): Option[Int] = {
    val range = inputs.head.split("-")
    val matching = (for (i <- range(0).toInt to range(1).toInt) yield check_criteria_wrapper_2(i)).toList
    Some(matching.map(x=> if(x) 1 else 0).sum)
  }

  def solve(inputs: List[String]): (Option[Int], Option[Int]) = {
    val res1 = part_1(inputs)
    val res2 = part_2(inputs)
    return (res1, res2)
  }
}
