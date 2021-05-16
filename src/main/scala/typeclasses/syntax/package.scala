package typeclasses

import typeclasses._

package object syntax
    extends ShowOps
    with ReadOps
    with MonadOps
    with ApplicativeOps
    with TraverseOps {

  object monad extends MonadOps

  object applicative extends ApplicativeOps

  object read extends ReadOps

  object show extends ShowOps

  object traverse extends TraverseOps

}
