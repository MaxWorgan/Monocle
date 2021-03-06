package monocle

import monocle.internal.{ Forget, Step, Strong }

import scalaz.Profunctor.UpStar
import scalaz.{ Applicative, Functor, Monoid, Profunctor, Tag }

/**
 * A Lens defines a single focus between a type S and A such as if you change A to B
 * you obtain a T.
 */
abstract class Lens[S, T, A, B] { self =>

  def _lens[P[_, _]: Strong]: Optic[P, S, T, A, B]

  @inline final def modifyF[F[_]: Functor](f: A => F[B])(s: S): F[T] =
    Tag.unwrap(_lens[UpStar[F, ?, ?]](Strong.upStarStrong[F])(UpStar[F, A, B](f))).apply(s)

  @inline final def get(s: S): A = _lens[Forget[A, ?, ?]].apply(Forget(identity)).runForget(s)

  @inline final def modify(f: A => B): S => T = _lens[Function1].apply(f)
  @inline final def set(b: B): S => T = modify(_ => b)

  // Compose
  @inline final def composeFold[C](other: Fold[A, C]): Fold[S, C] = asFold composeFold other
  @inline final def composeGetter[C](other: Getter[A, C]): Getter[S, C] = asGetter composeGetter other
  @inline final def composeSetter[C, D](other: Setter[A, B, C, D]): Setter[S, T, C, D] = asSetter composeSetter other
  @inline final def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asTraversal composeTraversal other
  @inline final def composeOptional[C, D](other: Optional[A, B, C, D]): Optional[S, T, C, D] = asOptional composeOptional other
  @inline final def composePrism[C, D](other: Prism[A, B, C, D]): Optional[S, T, C, D] = asOptional composeOptional other.asOptional
  final def composeLens[C, D](other: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
    @inline def _lens[P[_, _]: Strong]: Optic[P, S, T, C, D] = self._lens[P] compose other._lens[P]
  }
  final def composeIso[C, D](other: Iso[A, B, C, D]): Lens[S, T, C, D] = composeLens(other.asLens)

  // Optics transformation
  final def asFold: Fold[S, A] = new Fold[S, A] {
    @inline def foldMap[M: Monoid](f: A => M)(s: S): M = f(get(s))
  }
  @inline final def asGetter: Getter[S, A] = Getter[S, A](get)
  @inline final def asSetter: Setter[S, T, A, B] = Setter[S, T, A, B](modify)
  final def asTraversal: Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    @inline def _traversal[F[_]: Applicative](f: A => F[B])(s: S): F[T] = self.modifyF(f)(s)
  }
  final def asOptional: Optional[S, T, A, B] = new Optional[S, T, A, B] {
    @inline final def _optional[P[_, _]: Step]: Optic[P, S, T, A, B] = _lens[P]
  }

}

object Lens {

  def apply[S, T, A, B](_get: S => A)(_set: (B, S) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    @inline final def _lens[P[_, _]: Strong]: Optic[P, S, T, A, B] = pab =>
      Profunctor[P].dimap[(A, S), (B, S), S, T](Strong[P].first[A, B, S](pab))(s => (_get(s), s))(_set.tupled)
  }

}
