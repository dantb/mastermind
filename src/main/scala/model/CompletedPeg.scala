package model

import model.CodePeg.{ Blue, Green, Red, Yellow }
import typeclasses.Show

import scala.io.AnsiColor

final case class CompletedPeg(codePeg: CodePeg, keyPeg: KeyPeg)

object CompletedPeg:
  given Show[CompletedPeg] = new Show[CompletedPeg]:
    def show(t: CompletedPeg): String =
      val keyPeg = t.keyPeg match
        case KeyPeg.White => AnsiColor.WHITE ++ "@" ++ AnsiColor.RESET
        case KeyPeg.Black => AnsiColor.BLACK ++ "@" ++ AnsiColor.RESET
        case KeyPeg.Missed => ""
      s"${Show[CodePeg].show(t.codePeg)} $keyPeg"

  def white(codePeg: CodePeg): CompletedPeg = CompletedPeg(codePeg, KeyPeg.White)
  def black(codePeg: CodePeg): CompletedPeg = CompletedPeg(codePeg, KeyPeg.Black)
  def missed(codePeg: CodePeg): CompletedPeg = CompletedPeg(codePeg, KeyPeg.Missed)

end CompletedPeg
