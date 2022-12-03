package aoc2022.solutions

import aoc2022.solutions.Day3._

class Day3Spec extends munit.FunSuite:

  test("itemAppearingInBothCompartments") {
    assertEquals(itemAppearingInBothCompartments(Rucksack("vJrwpWtwJgWrhcsFMMfFFhFp")), 'p')
    assertEquals(itemAppearingInBothCompartments(Rucksack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")), 'L')
    assertEquals(itemAppearingInBothCompartments(Rucksack("PmmdzqPrVvPwwTWBwg")), 'P')
    assertEquals(itemAppearingInBothCompartments(Rucksack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")), 'v')
    assertEquals(itemAppearingInBothCompartments(Rucksack("ttgJtRGJQctTZtZT")), 't')
    assertEquals(itemAppearingInBothCompartments(Rucksack("CrZsJsPPZsGzwwsLwLmpwMDw")), 's')
  }

  test("scoreItem") {
    assertEquals(scoreItem('a'), 1)
    assertEquals(scoreItem('e'), 5)
    assertEquals(scoreItem('z'), 26)
    assertEquals(scoreItem('A'), 27)
    assertEquals(scoreItem('E'), 31)
    assertEquals(scoreItem('Z'), 52)
  }