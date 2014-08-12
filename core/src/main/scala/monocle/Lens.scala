package monocle

import scalaz.{Const, Applicative, Functor}

/**
 * A Lens defines a single focus between a type S and A such as if you change A to B
 * you obtain a T.
 */
abstract class Lens[S, T, A, B] private[monocle] { self =>

  def lift[F[_]: Functor](s: S, f: A => F[B]): F[T] // to define

  final def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] = asOptional.multiLift(from, f)

  final def get(s: S): A = lift[({ type l[b] = Const[A, b] })#l](s, { a: A => Const[A, B](a) }).getConst

  final def set(s: S, b: B): T = asSetter.set(s, b)
  final def setF(s: B): S => T = asSetter.setF(s)

  final def modify(s: S, f: A => B): T = asSetter.modify(s, f)
  final def modifyF(f: A => B): S => T = asSetter.modifyF(f)


  final lazy val asOptional = new Optional[S, T, A, B]{
    def multiLift[F[_] : Applicative](from: S, f: A => F[B]): F[T] = lift(from, f)
  }
  final lazy val asGetter = Getter[S, A](get)
  final def asTraversal   = asOptional.asTraversal
  final def asSetter      = asTraversal.asSetter
  final def asFold        = asTraversal.asFold

  final def composeOptional[C, D](other: Optional[A, B, C, D])  : Optional[S, T, C, D]  = asOptional  composeOptional  other
  final def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asTraversal composeTraversal other
  final def composeSetter[C, D](other: Setter[A, B, C, D])      : Setter[S, T, C, D]    = asSetter    composeSetter    other
  final def composeGetter(other: Getter[A, B])                  : Getter[S, B]          = asGetter    composeGetter    other
  final def composeFold(other: Fold[A, B])                      : Fold[S, B]            = asOptional  composeFold      other
  final def composePrism[C, D](other: Prism[A, B, C, D])        : Optional[S, T, C, D]  = asOptional  composeOptional  other.asOptional

  final def composeLens[C, D](other: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
    def lift[F[_]: Functor](from: S, f: C => F[D]): F[T] = self.lift(from, other.lift(_, f))
  }

  final def composeIso[C, D](other: Iso[A, B, C, D])            : Lens[S, T, C, D]      = composeLens(other.asLens)

}

object Lens {

  def apply[S, T, A, B](_get: S => A, _set: (S, B) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    def lift[F[_]: Functor](from: S, f: A => F[B]): F[T] =
      Functor[F].map(f(_get(from)))(newValue => _set(from, newValue))
  }

}
