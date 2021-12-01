package model

import effects.Sync
import typeclasses.{ Functor, Randomiser, Read, Show }

import scala.io.AnsiColor
import scala.util.Random

import typeclasses.syntax.functor.*

enum CodePeg:
  case Red, Green, Blue, Yellow

object CodePeg:
  // compiler warns about unreachable code for string match - this is awesome. Must be using singleton types.
  given Read[CodePeg] = str =>
    str.toLowerCase match
      case "r" => Some(Red)
      case "g" => Some(Green)
      case "b" => Some(Blue)
      case "y" => Some(Yellow)
      case _ => None

  given Show[CodePeg] = new Show[CodePeg]:
    def show(t: CodePeg): String = t match
      case Red => Show.red("O")
      case Green => Show.green("O")
      case Blue => Show.blue("O")
      case Yellow => Show.yellow("O")

  // grim to hardcode, surely we can get this from the enum ordinality?
  val Size: 4 = 4

  def random[F[_]: Randomiser: Functor]: F[CodePeg] =
    Randomiser[F].randomInt(Size).map(CodePeg.fromOrdinal)
