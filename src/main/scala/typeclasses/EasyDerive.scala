package typeclasses

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

/**
 * Trait can be used for simple derivation, by implementing deriveCaseClass and deriveSealed. Taken from:
 * https://blog.philipp-martini.de/blog/magic-mirror-scala3/
 */
trait EasyDerive[TC[_]] {
  def deriveCaseClass[A](caseClassType: CaseClassType[A]): TC[A]

  def deriveSealed[A](sealedType: SealedType[A]): TC[A]

  final def apply[A](using tc: TC[A]): TC[A] = tc

  case class CaseClassElement[A, B](
      label: String,
      typeclass: TC[B],
      getValue: A => B,
      idx: Int)

  case class CaseClassType[A](
      label: String,
      elements: List[
        CaseClassElement[A, _]
      ] /*, fromElements: List[Any] => A */)

  case class SealedElement[A, B](
      label: String,
      typeclass: TC[B],
      idx: Int,
      cast: A => B)

  case class SealedType[A](
      label: String,
      elements: List[SealedElement[A, _]],
      getElement: A => SealedElement[A, _])

  inline final def getInstances[A <: Tuple]: List[TC[Any]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[TC[t]].asInstanceOf[TC[Any]] :: getInstances[ts]
    }

  inline final def getElemLabels[A <: Tuple]: List[String] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].toString :: getElemLabels[ts]
    }

  inline final given derived[A](using m: Mirror.Of[A]): TC[A] = {
    val label = constValue[m.MirroredLabel]
    val elemInstances = getInstances[m.MirroredElemTypes]
    val elemLabels = getElemLabels[m.MirroredElemLabels]
    m match {
      case s: Mirror.SumOf[A] =>
        val elements = elemInstances.zip(elemLabels).zipWithIndex.map {
          case ((inst, lbl), idx) =>
            SealedElement[A, Any](
              lbl,
              inst.asInstanceOf[TC[Any]],
              idx,
              identity
            )
        }
        val getElement = (a: A) => elements(s.ordinal(a))
        deriveSealed(SealedType[A](label, elements, getElement))

      case p: Mirror.ProductOf[A] =>
        val caseClassElements: List[CaseClassElement[A, _]] =
          elemInstances.zip(elemLabels).zipWithIndex.map {
            case ((inst: TC[Any], lbl: String), idx: Int) =>
              CaseClassElement[A, Any](
                lbl,
                inst,
                (x: A) => x.asInstanceOf[scala.Product].productElement(idx),
                idx
              )
          }
//        val fromElements: List[Any] => A = { elements =>
//          val product: Product = new Product {
//            override def productArity: Int = caseClassElements.size
//
//            override def productElement(n: Int): Any = elements(n)
//
//            override def canEqual(that: Any): Boolean = false
//          }
//          p.fromProduct(product)
//        }
        deriveCaseClass(
          CaseClassType[A](label, caseClassElements /*, fromElements */ )
        )
    }
  }
}
