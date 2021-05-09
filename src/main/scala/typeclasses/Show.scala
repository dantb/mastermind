package typeclasses

trait Show[T]:
  def show(t: T): String

trait ShowOps:
  extension [T](t: T)(using s: Show[T]) def show: String = s.show(t)

object Show extends EasyDerive[Show] with ShowOps {

  given [T: Show]: Show[List[T]] = new Show[List[T]]:
    def show(ts: List[T]): String = s"List(${ts.map(_.show).mkString(", ")})"

  def deriveCaseClass[T](caseClassType: CaseClassType[T]): Show[T] = new Show[T]:
    def show(t: T): String =
      if (caseClassType.elements.isEmpty) caseClassType.label
      else
        caseClassType.elements.map { (p: CaseClassElement[T, _]) =>
          s"${p.label}=${p.typeclass.show(p.getValue(t))}"
        }.mkString(s"${caseClassType.label}(", ", ", ")")

  // Shouldn't the below be equivalent to SealedElement[T, _]? Doesn't seem to be.
  //  type Thing[T] = [Leaf] =>> SealedElement[T, Leaf]

  def deriveSealed[T](sealedType: SealedType[T]): Show[T] = new Show[T] {
    def show(t: T): String = {
      val elem: SealedElement[T, _] = sealedType.getElement(t)
      elem.typeclass.show(elem.cast(t))
    }
  }
}
