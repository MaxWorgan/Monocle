package monocle

import scalaz.{Functor, Applicative}

/**
 * Optional can be seen as a partial Lens - Lens toward an Option - or
 * a 0-1 Traversal. The latter constraint is not enforce at compile time
 * but by OptionalLaws
 */
abstract class Optional[S, T, A, B] private[monocle] { self =>

  def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] // to define

  final def getOption(from: S): Option[A] = asFold.headOption(from)

  final def set(from: S, newValue: B): T  = asTraversal.set(from, newValue)
  final def setF(b: B): S => T = asSetter.setF(b)
  final def setOption(from: S, newValue: B): Option[T] = modifyOption(from, _ => newValue)
  final def setOptionF(newValue: B): S => Option[T] = setOption(_, newValue)

  final def modify(from: S, f: A => B): T = asTraversal.modify(from, f)
  final def modifyF(f: A => B): S => T = asSetter.modifyF(f)
  final def modifyOption(from: S, f: A => B): Option[T] = getOption(from).map(a => set(from, f(a)))
  final def modifyOptionF(f: A => B): S => Option[T] = modifyOption(_, f)


  final lazy val asTraversal = new Traversal[S, T, A, B]{
    def multiLift[F[_] : Applicative](from: S, f: A => F[B]): F[T] = multiLift(from, f)
  }
  final def asSetter    = asTraversal.asSetter
  final def asFold      = asTraversal.asFold


  final def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asTraversal composeTraversal other
  final def composeSetter[C, D](other: Setter[A, B, C, D])      : Setter[S, T, C, D]    = asSetter    composeSetter    other
  final def composeFold(other: Fold[A, B])                      : Fold[S, B]            = asFold      composeFold      other

  final def composeOptional[C, D](other: Optional[A, B, C, D]): Optional[S, T, C, D] = new Optional[S, T, C, D] {
    def multiLift[F[_] : Applicative](from: S, f: C => F[D]): F[T] = self.multiLift(from, other.multiLift(_, f))
  }

  final def composeLens[C, D](other: Lens[A, B, C, D])  : Optional[S, T, C, D] = composeOptional(other.asOptional)
  final def composePrism[C, D](other: Prism[A, B, C, D]): Optional[S, T, C, D] = composeOptional(other.asOptional)
  final def composeIso[C, D](other: Iso[A, B, C, D])    : Optional[S, T, C, D] = composeOptional(other.asOptional)

}

object Optional {

  def apply[S, T, A, B](_getOption: S => Option[A], _set: (S, Option[B]) => T): Optional[S, T, A, B] = new Optional[S, T, A, B] {
    import scalaz.syntax.traverse._
    import scalaz.std.option._
    def multiLift[F[_] : Applicative](from: S, f: A => F[B]): F[T] =
     Functor[F].map(_getOption(from).map(f).sequence)(_set(from, _))
  }

}
