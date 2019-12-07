package com.cjhseq.adventofcode2019

import scala.math.abs

object Day03{

  type coordinate = (Int, Int)
  type segment = (coordinate, coordinate)

  private val direction = "(\\w)(\\d+)".r

  def update_position(new_direction: String, position: coordinate): coordinate = {
    val direction(orientation, length) = new_direction
    orientation match {
      case "U" => (position._1, position._2 + length.toInt)
      case "D" => (position._1, position._2 - length.toInt)
      case "L" => (position._1 - length.toInt, position._2)
      case "R" => (position._1 + length.toInt, position._2)
    }
  }

  def create_path(directions: List[String], position: coordinate = (0, 0)): List[coordinate] = {
    val current :: rest = directions
    val direction(orientation, length) = current
    val new_position = update_position(current, position)
    rest.length match {
      case 0 => List(position, new_position)
      case _ => position :: create_path(rest, new_position)
    }
  }

  def overlap_point(p1: coordinate, p2: coordinate, p3: coordinate, p4: coordinate): Option[coordinate] = {
    val sorted_x = List(p1._1, p2._1).sorted
    val sorted_y = List(p3._2, p4._2).sorted
    if ((p3._1 >= sorted_x(0)) && (p3._1 <= sorted_x(1)) && (sorted_y(0) <= p1._2) && (sorted_y(1) >= p1._2)){
      Some((p3._1, p1._2))
    } else {
      None
    }
  }

  def segments_cross(s1: segment, s2: segment): Option[coordinate] ={
    val (p1, p2) = s1
    val (p3, p4) = s2

    // (0, 0) (10, 0)
    // (5, 5) (5, -5)

    if ((p1._1 == p2._1) && (p3._2 == p4._2)){
      overlap_point(p3, p4, p1, p2)
    } else {
      if ((p1._1 != p2._1) && (p3._2 != p4._2)) {
        overlap_point(p1, p2, p3, p4)
      } else {
        None
      }
    }
  }

  def iterate_segments(s1: segment, path: List[coordinate]): List[coordinate] = {
    val current :: rest = path
    val seg_cross = segments_cross(s1, (current, rest.head)) match {
      case Some(x) => List(x)
      case _ => List[(Int, Int)]()
    }
    rest.length match {
      case 1 => seg_cross
      case _ => seg_cross ++ iterate_segments(s1, rest)
    }
  }

  def compare_two_paths(p1: List[coordinate], p2: List[coordinate]): List[coordinate] = {
    val current :: rest = p1
    val cross_points = iterate_segments((current, rest.head), p2)

    rest.length match {
      case 1 => cross_points
      case _ => cross_points ++ compare_two_paths(rest, p2)
    }
  }

  def calculate_manhattan(p: coordinate): Int = {
    abs(p._1) + abs(p._2)
  }

  def part_1(inputs: List[String]): Int = {
    val directions_1 = inputs(0).split(",").toList
    val directions_2 = inputs(1).split(",").toList
    val cross_overs = compare_two_paths(create_path(directions_1), create_path(directions_2)).tail // Tail here to drop the origin
    println(cross_overs)
    cross_overs.map(x=>calculate_manhattan(x)).min
  }

  def part_2(inputs: List[String]): Option[Int] = {

    Some(42)
  }

  def solve(inputs: List[String]): (Int, Option[Int]) = {
    val res1 = part_1(inputs)
    val res2 = part_2(inputs)
    return (res1, res2)
  }
}
