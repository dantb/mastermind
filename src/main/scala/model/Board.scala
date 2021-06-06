package model

import effects.Sync
import typeclasses.{Applicative, Monad, Randomiser, Read, Show}
import typeclasses.syntax.traverse._
import typeclasses.syntax.show._
import typeclasses.syntax.monad._
import typeclasses.syntax.tuple._

import compiletime.ops.int._
import scala.annotation.tailrec

final case class Board[Size <: Int](targetRow: CodeRow[Size], completedRows: List[CompletedRow[Size]])

type CodeRow[Size <: Int] = Fill[Size, CodePeg]

type CompletedRow[Size <: Int] = Fill[Size, CompletedPeg]

type Fill[Size <: Int, T] = Size match {
  case 3    => (T, T, T)
  case S[n] => T *: Fill[Size - 1, T]
}

object Board {

  def initialise[Size <: Int](targetRow: CodeRow[Size]): Board[Size] = Board(targetRow, Nil)

  def three(first: CodePeg, second: CodePeg, third: CodePeg): Board[3] = initialise((first, second, third))

  def four(first: CodePeg, second: CodePeg, third: CodePeg, fourth: CodePeg): Board[4] =
    initialise((first, second, third, fourth))

  def five(first: CodePeg, second: CodePeg, third: CodePeg, fourth: CodePeg, fifth: CodePeg): Board[5] =
      initialise((first, second, third, fourth, fifth))

  def randomThree[F[_]: Randomiser: Applicative]: F[Board[3]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F]).mapN(three)

  def randomFour[F[_]: Randomiser: Applicative]: F[Board[4]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F]).mapN(four)

  def randomFive[F[_]: Randomiser: Applicative]: F[Board[5]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F]).mapN(five)

  given [Size <: Int: ValueOf]: Show[Board[Size]] = new Show[Board[Size]]:
    def show(board: Board[Size]): String = {
      val completedRows: String = board.completedRows.foldLeft("") { case (acc, tuple) =>
        s"\n${Show[CompletedRow[Size]].show(tuple)}"
      }
      val targetRow: String = Show[CodeRow[Size]].show(board.targetRow)
      completedRows ++ "\n" ++ "|" ++ ("--" * summon[ValueOf[Size]].value) ++ "|\n" ++ s"|$targetRow |"
    }

  given [Size <: Int]: Show[CodeRow[Size]] = new Show[CodeRow[Size]]:
    def show(codeRow: CodeRow[Size]): String = codeRow match {
      case (t: CodePeg) *: (ts: CodeRow[Size - 1]) =>
        s"${t.show}${if (ts.isInstanceOf[EmptyTuple]) "" else " "}${Show[CodeRow[Size - 1]].show(ts)}"
      case _ => ""
    }

  given [Size <: Int]: Show[CompletedRow[Size]] = new Show[CompletedRow[Size]]:
    def show(codeRow: CompletedRow[Size]): String = codeRow match {
      case (t: CompletedPeg) *: (ts: CompletedRow[Size - 1]) =>
        s"${t.show}${if (ts.isInstanceOf[EmptyTuple]) "" else " "}${Show[CompletedRow[Size - 1]].show(ts)}"
      case _ => ""
    }

  given [Size <: Int: ValueOf]: Read[CodeRow[Size]] = new Read[CodeRow[Size]]:
    def read(str: String): Option[CodeRow[Size]] = {

      @tailrec
      def loop(chars: List[Char], tuple: Option[Tuple]): Option[Tuple] =
        chars match {
          case h :: tail => loop(tail, tuple.flatMap(t => Read[CodePeg].read(h.toString).map(cp => cp *: t)))
          case Nil       => tuple
        }

      // TODO: why doesn't the below compile? Why doesn't S[Sz] resolve to CodePeg *: CodeRow[Sz]?
//      @tailrec
//      def loop[Sz](chars: List[Char], tuple: Option[CodeRow[Sz]]): Option[CodeRow[Sz]] =
//        chars match {
//          case h :: tail => loop[S[Sz]](tail, tuple.flatMap(t => Read[CodePeg].read(h.toString).map(cp => cp *: t)))
//          case Nil       => tuple
//        }

      if   str.size != summon[ValueOf[Size]].value then None
      else loop(str.reverse.toList, Some(EmptyTuple)).asInstanceOf[Option[CodeRow[Size]]] // :sad:
    }
}


