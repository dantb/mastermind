package model

import effects.Sync
import typeclasses.{ Applicative, Monad, Randomiser, Read, Show }
import typeclasses.syntax.traverse.*
import typeclasses.syntax.show.*
import typeclasses.syntax.monad.*
import typeclasses.syntax.tuple.*

import compiletime.ops.int.*
import scala.annotation.tailrec

final case class Board[Size <: Int](
    targetRow: CodeRow[Size],
    completedRows: List[CompletedRow[Size]],
  )

type CodeRow[Size <: Int] = Fill[Size, CodePeg]

type CompletedRow[Size <: Int] = Fill[Size, CompletedPeg]

type Fill[Size <: Int, T] = Size match
  case 3 => (T, T, T)
  case S[n] => T *: Fill[Size - 1, T]

object Board:
  def initialise[Size <: Int](targetRow: CodeRow[Size]): Board[Size] = Board(targetRow, Nil)

  def three(
      first: CodePeg,
      second: CodePeg,
      third: CodePeg,
    ): Board[3] = initialise((first, second, third))

  def four(
      first: CodePeg,
      second: CodePeg,
      third: CodePeg,
      fourth: CodePeg,
    ): Board[4] =
    initialise((first, second, third, fourth))

  def five(
      first: CodePeg,
      second: CodePeg,
      third: CodePeg,
      fourth: CodePeg,
      fifth: CodePeg,
    ): Board[5] =
    initialise((first, second, third, fourth, fifth))

  def randomThree[F[_]: Randomiser: Applicative]: F[Board[3]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F]).mapN(three)

  def randomFour[F[_]: Randomiser: Applicative]: F[Board[4]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F]).mapN(four)

  def randomFive[F[_]: Randomiser: Applicative]: F[Board[5]] =
    (CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F], CodePeg.random[F])
      .mapN(five)

  given [Size <: Int: ValueOf]: Show[Board[Size]] = new Show[Board[Size]]:
    def show(board: Board[Size]): String =
      val completedRows: String = board.completedRows.foldLeft("") {
        case (acc, tuple) =>
          s"\n${Show[CompletedRow[Size]].show(tuple)}"
      }
      val targetRow: String = Show[CodeRow[Size]].show(board.targetRow)
      completedRows ++ "\n" ++ "|" ++ ("--" * summon[
        ValueOf[Size]
      ].value) ++ "|\n" ++ s"|$targetRow |"

  given [Size <: Int]: Show[CodeRow[Size]] = new Show[CodeRow[Size]]:
    def show(codeRow: CodeRow[Size]): String =
      def loop(pegs: List[CodePeg], acc: String): String =
        pegs match
          case Nil => acc
          case x :: Nil => acc ++ x.show
          case x :: xs => loop(xs, acc ++ x.show)
      loop(codeRow.asInstanceOf[Tuple].toList.asInstanceOf[List[CodePeg]], "")

  given [Size <: Int]: Show[CompletedRow[Size]] = new Show[CompletedRow[Size]]:
    def show(codeRow: CompletedRow[Size]): String =
      def loop(pegs: List[CompletedPeg], acc: String): String =
        pegs match
          case Nil => acc
          case x :: Nil => acc ++ x.show
          case x :: xs => loop(xs, acc ++ x.show)
      loop(codeRow.asInstanceOf[Tuple].toList.asInstanceOf[List[CompletedPeg]], "")

  given [Size <: Int: ValueOf]: Read[CodeRow[Size]] = new:
    def read(str: String): Option[CodeRow[Size]] =
      @tailrec
      def loop(chars: List[Char], tuple: Option[Tuple]): Option[Tuple] =
        chars match
          case h :: tail =>
            loop(tail, tuple.flatMap(t => Read[CodePeg].read(h.toString).map(cp => cp *: t)))
          case Nil => tuple

      // TODO: why doesn't the below compile? Why doesn't S[Sz] resolve to CodePeg *: CodeRow[Sz]?
//      @tailrec
//      def loop[Sz](chars: List[Char], tuple: Option[CodeRow[Sz]]): Option[CodeRow[Sz]] =
//        chars match {
//          case h :: tail => loop[S[Sz]](tail, tuple.flatMap(t => Read[CodePeg].read(h.toString).map(cp => cp *: t)))
//          case Nil       => tuple
//        }

      if str.size != summon[ValueOf[Size]].value then None
      else loop(str.reverse.toList, Some(EmptyTuple)).asInstanceOf[Option[CodeRow[Size]]] // :sad:

  def completeRow[Size <: Int](
      targetRow: CodeRow[Size],
      inputRow: CodeRow[Size],
    ): CompletedRow[Size] =
    val targetPegs: List[CodePeg] = targetRow.asInstanceOf[Tuple].toList.asInstanceOf[List[CodePeg]]
    val inputPegs: List[CodePeg] = inputRow.asInstanceOf[Tuple].toList.asInstanceOf[List[CodePeg]]
    completeRowLists(targetPegs, inputPegs)
      .reverse
      .foldLeft[Tuple](EmptyTuple) { (acc, next) =>
        next *: acc
      }
      .asInstanceOf[CompletedRow[Size]]

  private def completeRowLists(
      targetPegs: List[CodePeg],
      inputPegs: List[CodePeg],
    ): List[CompletedPeg] =
    val (blackPegs, nonBlackPegs) = targetPegs.zip(inputPegs).zipWithIndex.partition {
      case ((target, input), idx) => target == input
    }

    val (reds, greens, blues, yellows) = nonBlackPegs.map(_._1._1).foldLeft((0, 0, 0, 0)) {
      case ((r, g, b, y), next) =>
        next match
          case CodePeg.Red => (r + 1, g, b, y)
          case CodePeg.Green => (r, g + 1, b, y)
          case CodePeg.Blue => (r, g, b + 1, y)
          case CodePeg.Yellow => (r, g, b, y + 1)
    }

    final case class CompletedWithIndex(peg: CompletedPeg, index: Int)

    val completedBlackPegs: List[CompletedWithIndex] = blackPegs.map {
      case ((peg, _), idx) => CompletedWithIndex(CompletedPeg(peg, KeyPeg.Black), idx)
    }
    val completedNonBlackPegs: List[CompletedWithIndex] = nonBlackPegs
      .foldLeft((List.empty[CompletedWithIndex], reds, greens, blues, yellows)) {
        case ((acc, r, g, b, y), ((_, inputPeg), idx)) =>
          def hitOrMiss(count: Int): KeyPeg = if count > 0 then KeyPeg.White else KeyPeg.Missed
          inputPeg match
            case CodePeg.Red =>
              (CompletedWithIndex(CompletedPeg(inputPeg, hitOrMiss(r)), idx) :: acc, r - 1, g, b, y)
            case CodePeg.Green =>
              (CompletedWithIndex(CompletedPeg(inputPeg, hitOrMiss(g)), idx) :: acc, r, g - 1, b, y)
            case CodePeg.Blue =>
              (CompletedWithIndex(CompletedPeg(inputPeg, hitOrMiss(b)), idx) :: acc, r, g, b - 1, y)
            case CodePeg.Yellow =>
              (CompletedWithIndex(CompletedPeg(inputPeg, hitOrMiss(y)), idx) :: acc, r, g, b, y - 1)
      }
      ._1

    (completedBlackPegs ++ completedNonBlackPegs).sortBy(_.index).map(_.peg)
