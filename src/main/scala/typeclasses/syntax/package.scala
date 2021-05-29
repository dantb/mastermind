package typeclasses

import typeclasses._

package object syntax {

  object monad extends MonadOps

  object applicative extends ApplicativeOps

  object read extends ReadOps

  object show extends ShowOps

  object traverse extends TraverseOps

  object tuple extends TupleOps

  object functor extends FunctorOps

  object all
      extends ShowOps
      with ReadOps
      with MonadOps
      with ApplicativeOps
      with TraverseOps
      with TupleOps
      with FunctorOps

}
