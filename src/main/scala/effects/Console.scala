package effects

trait Console[F[_]]:
  def read: F[String]
  def write(str: String): F[Unit]

trait ConsoleOps:
  extension [F[_]: Console](s: String) def write: F[Unit] = Console[F].write(s)

object Console {
  def apply[F[_]](using c: Console[F]) = c

}
