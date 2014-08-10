package monocle

import scalaz.{Const, Applicative, Functor}

/**
 * A Lens defines a single focus between a type S and A such as if you change A to B
 * you obtain a T.
 */
abstract class Lens[S, T, A, B] private[monocle] { self =>

  def lift[F[_]: Functor](from: S, f: A => F[B]): F[T] // to define

  lazy val asGetter = Getter[S, A](get)

  lazy val asOptional = new Optional[S, T, A, B]{
    def multiLift[F[_] : Applicative](from: S, f: A => F[B]): F[T] = lift(from, f)
  }
  lazy val asTraversal = asOptional.asTraversal

  def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] = asOptional.multiLift(from, f)

  def get(from: S): A = lift[({ type l[b] = Const[A, b] })#l](from, { a: A => Const[A, B](a) }).getConst

  def set(from: S, newValue: B): T  = asOptional.set(from, newValue)
  def modify(from: S, f: A => B): T = asOptional.modify(from, f)

  final def setF(newValue: B): S => T = set(_, newValue)
  final def modifyF(f: A => B): S => T = modify(_, f)

  def compose[C, D](other: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
    def lift[F[_]: Functor](from: S, f: C => F[D]): F[T] = self.lift(from, other.lift(_, f))
  }

  def compose[C, D](other: Iso[A, B, C, D])      : Lens[S, T, C, D]      = compose(other.asLens)
  def compose[C, D](other: Prism[A, B, C, D])    : Optional[S, T, C, D]  = asOptional compose other

  def compose[C, D](other: Optional[A, B, C, D]) : Optional[S, T, C, D]  = asOptional compose other
  def compose[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asOptional compose other
  def compose[C, D](other: Setter[A, B, C, D])   : Setter[S, T, C, D]    = asOptional compose other
  def compose(other: Getter[A, B])               : Getter[S, B]          = asGetter   compose other
  def compose(other: Fold[A, B])                 : Fold[S, B]            = asOptional compose other

}

object Lens {

  def apply[S, T, A, B](_get: S => A, _set: (S, B) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    def lift[F[_]: Functor](from: S, f: A => F[B]): F[T] =
      Functor[F].map(f(_get(from)))(newValue => _set(from, newValue))
  }

}
