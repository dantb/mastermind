package model

import effects.Sync
import typeclasses.{Applicative, Monad, Randomiser, Read, Show}
import typeclasses.syntax.traverse._
import typeclasses.syntax.show._
import typeclasses.syntax.monad._
import typeclasses.syntax.tuple._

import compiletime.ops.int._

final case class Board[Size <: Int](targetRow: CodeRow[Size], completedRows: List[CompletedRow[Size]])

type CodeRow[Size <: Int] = Fill[Size, CodePeg]

type CompletedRow[Size <: Int] = Fill[Size, CompletedPeg]

type Fill[Size <: Int, T] = Size match {
  case 0    => EmptyTuple
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

  given Show[Board[4]] = new Show[Board[4]]:
    def show(board: Board[4]): String = board.targetRow match {
      case (a, b, c, d) =>
        val completedRows: String = board.completedRows.foldLeft("") { case (acc, (a, b, c, d)) =>
          s"\n${a.show} ${b.show} ${c.show} ${d.show}"
        }
        val targetRow = s"${a.show} ${b.show} ${c.show} ${d.show}"
        completedRows ++ "\n" ++ ("--" * 4) ++ "\n" ++ targetRow

    }
}


