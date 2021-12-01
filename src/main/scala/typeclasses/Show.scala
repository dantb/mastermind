package typeclasses

import scala.io.AnsiColor

trait Show[T]:
  def show(t: T): String

trait ShowOps:
  extension [T](t: T)(using s: Show[T]) def show: String = s.show(t)

object Show extends EasyDerive[Show] with ShowOps:

  def red(str: String) = AnsiColor.RED ++ str ++ AnsiColor.RESET
  def green(str: String) = AnsiColor.GREEN ++ str ++ AnsiColor.RESET
  def blue(str: String) = AnsiColor.BLUE ++ str ++ AnsiColor.RESET
  def yellow(str: String) = AnsiColor.YELLOW ++ str ++ AnsiColor.RESET

  given [T: Show]: Show[List[T]] = new Show[List[T]]:
    def show(ts: List[T]): String = s"List(${ts.map(_.show).mkString(", ")})"

  def deriveCaseClass[T](caseClassType: CaseClassType[T]): Show[T] = new Show[T]:
    def show(t: T): String =
      if (caseClassType.elements.isEmpty) caseClassType.label
      else
        caseClassType.elements.map { (p: CaseClassElement[T, ?]) =>
          s"${p.label}=${p.typeclass.show(p.getValue(t))}"
        }.mkString(s"${caseClassType.label}(", ", ", ")")

  // Shouldn't the below be equivalent to SealedElement[T, _]? Doesn't seem to be.
  //  type Thing[T] = [Leaf] =>> SealedElement[T, Leaf]

  def deriveSealed[T](sealedType: SealedType[T]): Show[T] = new Show[T] {
    def show(t: T): String =
      val elem: SealedElement[T, ?] = sealedType.getElement(t)
      elem.typeclass.show(elem.cast(t))
  }
