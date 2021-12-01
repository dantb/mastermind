package model

import munit.FunSuite
import typeclasses.Read
import model.Board.*
import model.Board
import model.Board.given
import CodePeg.*
import typeclasses.Show

class BoardSpec extends munit.FunSuite:

  // What's going on, why don't these resolve themselves?
  given Show[CodeRow[4]] = Board.given_Show_CodeRow[4]
  given Read[CodeRow[4]] = Board.given_Read_CodeRow[4]

  test("Show[CodeRow[4]] should be able to show a string from a CodeRow[4]") {
    val input = (CodePeg.Red, CodePeg.Green, CodePeg.Blue, CodePeg.Yellow)
    val expected = Show.red("O") ++ Show.green("O") ++ Show.blue("O") ++ Show.yellow("O")
    assertEquals(summon[Show[CodeRow[4]]].show(input), expected)
  }

  test("Read[CodeRow[4]] should be able to parse a string into a CodeRow[4]") {
    val expected = (CodePeg.Red, CodePeg.Green, CodePeg.Blue, CodePeg.Yellow)
    assertEquals(Read[CodeRow[4]].read("rgby"), Some(expected))
  }

  test("Read[CodeRow[5]] should be able to parse a string into a CodeRow[5]") {
    given Read[CodeRow[5]] = Board.given_Read_CodeRow[5]
    val expected = (CodePeg.Yellow, CodePeg.Yellow, CodePeg.Blue, CodePeg.Yellow, CodePeg.Green)
    assertEquals(Read[CodeRow[5]].read("yybyg"), Some(expected))
  }

  test("Read[CodeRow[4]] should return None for too large an input") {
    assertEquals(Read[CodeRow[4]].read("rgbyr"), None)
  }

  test("Read[CodeRow[4]] should return None for too small an input") {
    assertEquals(Read[CodeRow[4]].read("rgb"), None)
  }

  test("Read[CodeRow[4]] should return None for invalid input") {
    assertEquals(Read[CodeRow[4]].read("rgbx"), None)
  }

  test("Board.completeRow identifies a fully correct row") {
    assertEquals(
      Board.completeRow[4]((Red, Red, Green, Blue), (Red, Red, Green, Blue)),
      (CompletedPeg.black(Red), CompletedPeg.black(Red), CompletedPeg.black(Green), CompletedPeg.black(Blue))
    )
  }

  test("Board.completeRow identifies two out of order pegs") {
    assertEquals(
      Board.completeRow[4]((Red, Red, Green, Blue), (Red, Red, Blue, Green)),
      (CompletedPeg.black(Red), CompletedPeg.black(Red), CompletedPeg.white(Blue), CompletedPeg.white(Green))
    )
  }

  test("Board.completeRow identifies a miss when appearing as black in another position") {
    assertEquals(
      Board.completeRow[3]((Yellow, Red, Blue), (Red, Red, Blue)),
      (CompletedPeg.missed(Red), CompletedPeg.black(Red), CompletedPeg.black(Blue))
    )
    assertEquals(
      Board.completeRow[3]((Red, Yellow, Blue), (Red, Red, Blue)),
      (CompletedPeg.black(Red), CompletedPeg.missed(Red), CompletedPeg.black(Blue))
    )
  }

  test("Board.completeRow identifies correct colours without positions") {
    assertEquals(
      Board.completeRow[3]((Yellow, Yellow, Red), (Yellow, Red, Yellow)),
      (CompletedPeg.black(Yellow), CompletedPeg.white(Red), CompletedPeg.white(Yellow))
    )
    assertEquals(
      Board.completeRow[3]((Red, Yellow, Red), (Yellow, Red, Yellow)),
      (CompletedPeg.white(Yellow), CompletedPeg.white(Red), CompletedPeg.missed(Yellow))
    )
  }

