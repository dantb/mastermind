package typeclasses

trait TupleOps:

  extension [F[_], A, B](tuple: Tuple2[F[A], F[B]])
    def mapN[C](f: (A, B) => C)(using Applicative[F]): F[C] =
      Applicative[F].map(tuple.product)(f.tupled)
    def product(using Applicative[F]): F[(A, B)] =
      Applicative[F].product(tuple._1, tuple._2)

  extension [F[_], A, B, C](tuple: Tuple3[F[A], F[B], F[C]])
    def mapN[D](f: (A, B, C) => D)(using Applicative[F]): F[D] =
      Applicative[F].map(tuple.product)(f.tupled)
    def product(using Applicative[F]): F[(A, B, C)] = tuple match {
      case (a, b, c) =>
        Applicative[F].map(((a, b).product, c).product) {
          case ((a, b), c) => (a, b, c)
        }
    }

  extension [F[_], A, B, C, D](tuple: Tuple4[F[A], F[B], F[C], F[D]])
    def mapN[E](f: (A, B, C, D) => E)(using Applicative[F]): F[E] =
      Applicative[F].map(tuple.product)(f.tupled)
    def product(using Applicative[F]): F[(A, B, C, D)] = tuple match {
      case (a, b, c, d) =>
        Applicative[F].map((((a, b).product, c).product, d).product) {
          case (((a, b), c), d) => (a, b, c, d)
        }
    }

  extension [F[_], A, B, C, D, E](tuple: Tuple5[F[A], F[B], F[C], F[D], F[E]])
    def mapN[G](f: (A, B, C, D, E) => G)(using Applicative[F]): F[G] =
      Applicative[F].map(tuple.product)(f.tupled)
    def product(using Applicative[F]): F[(A, B, C, D, E)] = tuple match {
      case (a, b, c, d, e) =>
        Applicative[F].map(((((a, b).product, c).product, d).product, e).product) {
          case ((((a, b), c), d), e) => (a, b, c, d, e)
        }
    }
