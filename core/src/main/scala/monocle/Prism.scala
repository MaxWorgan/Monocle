package monocle

import scalaz.{Applicative, \/}

/**
 * A Prism is a special case of Traversal where the focus is limited to
 * 0 or 1 A. In addition, a Prism defines a reverse relation such as
 * you can always get T from B.
 */
final class Prism[S, T, A, B] private[monocle] (_reverseGet: B => T, val seta: S => T \/ A) {

  lazy val asOptional = new Optional[S, T, A, B]{
    def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] =
      seta(from) // T \/ A
        .map(f) // T \/ F[B]
        .map(Applicative[F].map(_)(_reverseGet)) // T \/ F[T]
        .leftMap(Applicative[F].point(_)) // F[T] \/ F[T]
        .fold(identity, identity) // F[T]
  }

  lazy val asTraversal = asOptional.asTraversal

  def re = Getter[B, T](_reverseGet)
  def reverseGet(from: B): T = re.get(from)

  def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] = asOptional.multiLift(from, f)

  def getOption(from: S): Option[A] = asOptional.getOption(from)
  def set(from: S, newValue: B): T  = asOptional.set(from, newValue)
  def modify(from: S, f: A => B): T = asOptional.modify(from, f)

  def compose[C, D](other: Prism[A, B, C, D]): Prism[S, T, C, D] =
    new Prism[S, T, C, D](
      reverseGet _ compose other.reverseGet,
      s => seta(s) flatMap( a => other.seta(a) leftMap _reverseGet )
    )

  def compose[C, D](other: Iso[A, B, C, D]) : Prism[S, T, C, D]    = compose(other.asPrism)
  def compose[C, D](other: Lens[A, B, C, D]): Optional[S, T, C, D] = asOptional.compose(other.asOptional)

  def compose[C, D](other: Optional[A, B, C, D]) : Optional[S, T, C, D]  = asOptional compose other
  def compose[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asOptional compose other
  def compose[C, D](other: Setter[A, B, C, D])   : Setter[S, T, C, D]    = asOptional compose other
  def compose(other: Fold[A, B])                 : Fold[S, B]            = asOptional compose other

}

object Prism {

  def apply[S, T, A, B](_reverseGet: B => T, seta: S => T \/ A): Prism[S, T, A, B] =
    new Prism[S, T, A, B](_reverseGet, seta)

}
