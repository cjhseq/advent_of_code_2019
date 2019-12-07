package com.cjhseq.adventofcode2019


object Day05{

  def get_values(ints: Array[Int], intcode: Array[Int], param_1: Int, param_2: Int): (Int, Int) = {
    val value_1 = if (param_1 == 0) ints(intcode(1)) else intcode(1)
    val value_2 = if (param_2 == 0) ints(intcode(2)) else intcode(2)
    (value_1, value_2)
  }

  def jump_lookup(ints: Array[Int], intcode: Array[Int], param_1: Int, param_2: Int, jump_type: Boolean): Int = {
    val (value_1, value_2) = get_values(ints, intcode, param_1, param_2)
    if ((value_1 != 0) == jump_type) value_2 else -1
  }

  def less_than(ints: Array[Int], intcode: Array[Int], param_1: Int, param_2: Int): Int = {
    val (value_1, value_2) = get_values(ints, intcode, param_1, param_2)
    if (value_1 < value_2) 1 else 0
  }

  def equals(ints: Array[Int], intcode: Array[Int], param_1: Int, param_2: Int): Int = {
    val (value_1, value_2) = get_values(ints, intcode, param_1, param_2)
    if (value_1 == value_2) 1 else 0
  }

  def cycle_intcode(ints: Array[Int], position: Int = 0): Array[Int] = {
    val outputs = if (position == 0) ints.clone() else ints
    val current = outputs.take(position + 4).drop(position)
    val opcode = current(0) % 100
    val param_1 = (current(0)/100) % 10
    val param_2 = (current(0)/1000) % 10
    val increment = opcode match {
      case 1 => 4
      case 2 => 4
      case 3 => 2
      case 4 => 2
      case 5 => 3
      case 6 => 3
      case 7 => 4
      case 8 => 4
      case 99 => 0
    }

    if (Set(1,2,7,8) contains opcode) {
      val value_1 = if (param_1 == 0) outputs(current(1)) else current(1)
      val value_2 = if (param_2 == 0) outputs(current(2)) else current(2)
      outputs(current(3)) = opcode match {
        case 1 => value_1 + value_2
        case 2 => value_1 * value_2
        case 7 => less_than(outputs, current, param_1, param_2)
        case 8 => equals(outputs, current, param_1, param_2)
      }
    }

    if (opcode == 3){
      println("Please enter value")
      outputs(current(1)) = scala.io.StdIn.readInt()
    }

    if (opcode == 4){
      val value_1 = if (param_1 == 0) outputs(current(1)) else current(1)
      println(value_1)
    }

    val jump = opcode match {
      case 5 => jump_lookup(outputs, current, param_1, param_2, true)
      case 6 => jump_lookup(outputs, current, param_1, param_2, false)
      case _ => -1
    }
    val jump_adjust = if (jump >= 0) jump - position - increment else 0

    current(0) match {
      case 99 => outputs
      case _ => cycle_intcode(outputs, position + increment + jump_adjust)
    }



  }

  def part_1(inputs: List[String]): String = {
    val ints = inputs.head.split(",").map(_.toInt).toArray // Normally inputs are newline separated, this time no
    //ints(1)=12
    //ints(2)=2
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
    val res2 = (42, 42)
    return (res1, res2)
  }
}
