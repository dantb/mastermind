package typeclasses

trait Read[T]:
  def read(s: String): Option[T]

trait ReadOps:
  extension [T](s: String)(using r: Read[T]) def read: Option[T] = r.read(s)

object Read {
  extension [T: Read](s: String) def read: Option[T] = summon[Read[T]].read(s)
}
