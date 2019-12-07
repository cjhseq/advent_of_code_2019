package com.cjhseq.adventofcode2019


object Day02{

  def cycle_intcode(ints: Array[Int], iteration: Int = 0): Array[Int] = {
    val outputs = if (iteration == 0) ints.clone() else ints
    val to_take = iteration * 4
    val current = outputs.take(to_take + 4).drop(to_take)

    if (current(0)!= 99) {
      outputs(current(3)) = current(0)  match {
        case 1 => outputs(current(1)) + outputs(current(2))
        case 2 => outputs(current(1)) * outputs(current(2))
      }
    }

    current(0) match {
      case 99 => outputs
      case _ => cycle_intcode(outputs, iteration + 1)
    }
  }

  def part_1(inputs: List[String]): String = {
    val ints = inputs.head.split(",").map(_.toInt).toArray // Normally inputs are newline separated, this time no
    ints(1)=12
    ints(2)=2
    val res = cycle_intcode(ints)
    return(res.slice(0,5).mkString(","))

  }

  def part_2(inputs: List[String]): (Int, Int) = {
    val ints = inputs.head.split(",").map(_.toInt).toArray // Normally inputs are newline separated, this time no
    var i = -1
    var j = -1
    var done = false
    while (!done && (i<100)) {
      i += 1
      j = -1
      while (!done && (j<100)) {
        j += 1
        ints(1)=i
        ints(2)=j
        val res = cycle_intcode(ints)
        if (res(0) == 19690720) done = true
      }
    }
    return (i, j)
  }

  def solve(inputs: List[String]): (String, (Int, Int)) = {
    val res1 = part_1(inputs)
    val res2 = part_2(inputs)
    return (res1, res2)
  }
}
