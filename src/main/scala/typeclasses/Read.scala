package typeclasses

trait Read[T]:
  def read(s: String): Option[T]

trait ReadOps:
  extension [T](s: String)(using r: Read[T]) def read: Option[T] = r.read(s)

object Read {
  def apply[T: Read]: Read[T] = summon[Read[T]]
}
