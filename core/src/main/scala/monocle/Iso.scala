package monocle

import scalaz.{Applicative, \/-, Functor}

/**
 * An Iso is a Lens that can be reversed and so it defines an isomorphism.
 */
final class Iso[S, T, A, B] private[monocle] (_get: S => A, _reverseGet: B => T) {

  def reverse: Iso[B, A, T, S] = new Iso[B, A, T, S](_reverseGet, _get)

  def lift[F[_]: Functor](from: S, f: A => F[B]): F[T] = asLens.lift(from, f)
  def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] = asLens.multiLift(from, f)

  def get(from: S): A = asLens.get(from)

  def set(s: S, b: B): T = asTraversal.set(s, b)
  def setF(s: B): S => T = asTraversal.setF(s)

  def modify(s: S, f: A => B): T = asTraversal.modify(s, f)
  def modifyF(f: A => B): S => T = asTraversal.modifyF(f)

  def reverseGet(from: B): T = _reverseGet(from)
  def re: Getter[B, T] = asPrism.re

  lazy val asLens  = Lens[S, T, A, B](_get, (_, b) => _reverseGet(b))
  lazy val asPrism = Prism[S, T, A, B](_reverseGet, s => \/-(_get(s)) )
  def asOptional   = asLens.asOptional
  def asTraversal  = asOptional.asTraversal
  def asSetter     = asTraversal.asSetter
  def asGetter     = asLens.asGetter
  def asFold       = asLens.asFold


  def composePrism[C, D](other: Prism[A, B, C, D])        : Prism[S, T, C, D]     = asPrism     composePrism     other
  def composeLens[C, D](other: Lens[A, B, C, D])          : Lens[S, T, C, D]      = asLens      composeLens      other
  def composeOptional[C, D](other: Optional[A, B, C, D])  : Optional[S, T, C, D]  = asOptional  composeOptional  other
  def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asTraversal composeTraversal other
  def composeSetter[C, D](other: Setter[A, B, C, D])      : Setter[S, T, C, D]    = asSetter    composeSetter    other
  def composeGetter(other: Getter[A, B])                  : Getter[S, B]          = asGetter    composeGetter    other
  def composeFold(other: Fold[A, B])                      : Fold[S, B]            = asFold      composeFold      other

  def composeIso[C, D](other: Iso[A, B, C, D]): Iso[S, T, C, D] =
    new Iso[S, T, C, D](other.get _ compose get, _reverseGet compose other.reverseGet)

}

object Iso {

  def apply[S, T, A, B](_get: S => A, _reverseGet: B => T): Iso[S, T, A, B] =
    new Iso[S, T, A, B](_get, _reverseGet)

}
