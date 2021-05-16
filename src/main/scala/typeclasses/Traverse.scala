package typeclasses

trait Traverse[F[_]]:
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse[G, G[A], A](fga)(identity)
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

trait TraverseOps:
  extension [F[_]: Traverse, A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = Traverse[F].traverse(fa)(f)

  extension [F[_]: Traverse, G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] = Traverse[F].sequence(fga)


object Traverse extends ApplicativeOps {
  def apply[F[_]](using t: Traverse[F]): Traverse[F] = t

  given Traverse[List] = new Traverse[List]:
   def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
     fa.foldRight(Applicative[G].pure(Nil)) { (a, acc) =>
       acc.product(f(a)).map((accc, aa) => aa :: accc)
     }

}
