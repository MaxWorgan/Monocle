package monocle

import scalaz.{Applicative, \/-, Functor}

/**
 * An Iso is a Lens that can be reversed and so it defines an isomorphism.
 */
final class Iso[S, T, A, B] private[monocle] (_get: S => A, _reverseGet: B => T) {

  lazy val asLens  = Lens[S, T, A, B](_get, (_, b) => _reverseGet(b))
  lazy val asPrism = Prism[S, T, A, B](_reverseGet, s => \/-(_get(s)) )
  lazy val asOptional = asLens.asOptional
  lazy val asTraversal = asLens.asTraversal

  def reverse: Iso[B, A, T, S] = new Iso[B, A, T, S](_reverseGet, _get)

  def lift[F[_]: Functor](from: S, f: A => F[B]): F[T] = asLens.lift(from, f)
  def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] = asLens.multiLift(from, f)

  def get(from: S): A = asLens.get(from)
  def set(from: S, newValue: B): T = asLens.set(from, newValue)
  def modify(from: S, f: A => B): T = asLens.modify(from, f)

  def reverseGet(from: B): T = _reverseGet(from)
  def re: Getter[B, T] = asPrism.re

  def compose[C, D](other: Iso[A, B, C, D]): Iso[S, T, C, D] =
    new Iso[S, T, C, D](other.get _ compose get, _reverseGet compose other.reverseGet)

  def compose[C, D](other: Prism[A, B, C, D])    : Prism[S, T, C, D]     = asPrism compose other
  def compose[C, D](other: Lens[A, B, C, D])     : Lens[S, T, C, D]      = asLens compose other
  def compose[C, D](other: Optional[A, B, C, D]) : Optional[S, T, C, D]  = asLens compose other
  def compose[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asLens compose other
  def compose[C, D](other: Setter[A, B, C, D])   : Setter[S, T, C, D]    = asLens compose other
  def compose(other: Getter[A, B])               : Getter[S, B]          = asLens compose other
  def compose(other: Fold[A, B])                 : Fold[S, B]            = asLens compose other
}

object Iso {

  def apply[S, T, A, B](_get: S => A, _reverseGet: B => T): Iso[S, T, A, B] =
    new Iso[S, T, A, B](_get, _reverseGet)

}
