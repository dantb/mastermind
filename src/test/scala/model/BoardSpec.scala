package model

import munit.FunSuite
import typeclasses.Read
import model.Board.given

class BoardSpec extends munit.FunSuite {

  test("Read[CodeRow[4]] should be able to parse a string into a CodeRow[4]") {
    val expected = (CodePeg.Red, CodePeg.Green, CodePeg.Blue, CodePeg.Yellow)
    assertEquals(Read[CodeRow[4]].read("rgby"), Some(expected))
  }

  test("Read[CodeRow[5]] should be able to parse a string into a CodeRow[5]") {
    val expected = (CodePeg.Yellow, CodePeg.Yellow, CodePeg.Blue, CodePeg.Yellow, CodePeg.Green)
    assertEquals(Read[CodeRow[4]].read("yybyg"), None)
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

}
