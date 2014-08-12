package monocle


final class Setter[S, T, A, B] private (_modifyF: (A => B) => S => T) {

  def set(s: S, b: B): T = setF(b)(s)
  def setF(b: B): S => T = modifyF(_ => b)

  def modify(s: S, f: A => B): T = modifyF(f)(s)
  def modifyF(f: A => B): S => T = _modifyF(f)

  def composeSetter[C, D](other: Setter[A, B, C, D]): Setter[S, T, C, D] =
    new Setter[S, T, C, D](_modifyF compose other.modifyF)

  def composeTraversal[C, D](other: Traversal[A, B, C, D]): Setter[S, T, C, D] = composeSetter(other.asSetter)
  def composeOptional[C, D](other: Optional[A, B, C, D])  : Setter[S, T, C, D] = composeSetter(other.asSetter)
  def composePrism[C, D](other: Prism[A, B, C, D])        : Setter[S, T, C, D] = composeSetter(other.asSetter)
  def composeLens[C, D](other: Lens[A, B, C, D])          : Setter[S, T, C, D] = composeSetter(other.asSetter)
  def composeIso[C, D](other: Iso[A, B, C, D])            : Setter[S, T, C, D] = composeSetter(other.asSetter)

}

object Setter {

  import scalaz.Functor

  def apply[S, T, A, B](_modifyF: (A => B) => S => T) =
    new Setter[S, T, A, B](_modifyF)

  def apply[F[_]: Functor, A, B]: Setter[F[A], F[B], A, B] =
    new Setter[F[A], F[B], A, B](Functor[F].lift _)

}
