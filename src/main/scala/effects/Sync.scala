package effects

trait Sync[F[_]]:
  def delay[A](a: => A): F[A]

object Sync:
  def apply[F[_]](using s: Sync[F]): Sync[F] = s
