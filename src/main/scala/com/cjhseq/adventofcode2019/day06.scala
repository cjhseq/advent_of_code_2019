package com.cjhseq.adventofcode2019


object Day06{

  class SpaceThing(val name: String, val orbits: Option[SpaceThing] = None){

  }

  def find_path(o1: List[String], o2: List[String]): Int = {
    val s1 :: rest1 = o1
    val s2 :: rest2 = o2
    s1 == s2 match {
      case false => rest1.length + rest2.length + 2
      case true => find_path(rest1, rest2)
    }
  }

  def list_orbits(space_thing: SpaceThing, orbits: List[String] = List()): List[String] = {
    space_thing.orbits match {
      case None => orbits
      case Some(x) => list_orbits(x, x.name :: orbits)
    }
  }

  def count_orbits(space_thing: SpaceThing, count: Int = 0): Int = {
    space_thing.orbits match {
      case None => count
      case Some(x) => count_orbits(x, count+1)
    }
  }

  def construct_space_map(to_create: String, orbit_map: Map[String, String], space_things: Map[String, SpaceThing] = Map()): Map[String, SpaceThing] = {
    val orbiting = orbit_map(to_create)
    val exists = space_things.keySet
    if (exists contains to_create){
      return(space_things)
    }

    if ((exists contains orbiting) || (orbiting == "None")){
      val orbit_thing: Option[SpaceThing] = if (orbiting == "None") None else Some(space_things(orbiting))
      space_things updated (to_create, new SpaceThing(to_create, orbit_thing))
    } else {
      val updated_space_things = space_things ++ construct_space_map(orbiting, orbit_map, space_things)
      space_things updated (to_create, new SpaceThing(to_create, Some(updated_space_things(orbiting))))
    }
  }

  def split_mapping(mapping: String): (String, String) ={
    val map_regex = "(\\w+)\\)(\\w+)".r
    val map_regex(k, v) = mapping
    (k, v)
  }

  def prep_orbit_map(inputs: List[String]): Map[String, String] = {
    inputs.map(x => split_mapping(x)).foldLeft(Map[String, String]()) {(m, y) => m + (y._2 -> y._1) } + ("COM" -> "None")
  }

  def part_1(inputs: List[String]): Int = {
    val orbit_map = prep_orbit_map(inputs)
    val space_map = orbit_map.keys.foldLeft(Map[String, SpaceThing]()) { (m, x) => m ++ construct_space_map(x, orbit_map) }

    space_map.values.map(x=>count_orbits(x)).sum
  }

  def part_2(inputs: List[String]): Int = {
    val orbit_map = prep_orbit_map(inputs)
    val space_map = orbit_map.keys.foldLeft(Map[String, SpaceThing]()) { (m, x) => m ++ construct_space_map(x, orbit_map) }

    find_path(list_orbits(space_map("YOU")), list_orbits(space_map("SAN")))
  }

  def solve(inputs: List[String]): (Int, Int) = {
    val res1 = part_1(inputs)
    val res2 = part_2(inputs)
    return (res1, res2)
  }
}
