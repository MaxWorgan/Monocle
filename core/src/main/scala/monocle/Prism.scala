package monocle

import scalaz.{Applicative, \/}

/**
 * A Prism is a special case of Traversal where the focus is limited to
 * 0 or 1 A. In addition, a Prism defines a reverse relation such as
 * you can always get T from B.
 */
final class Prism[S, T, A, B] private[monocle] (_reverseGet: B => T, val seta: S => T \/ A) {

  def re = Getter[B, T](_reverseGet)
  def reverseGet(s: B): T = re.get(s)

  def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] = asTraversal.multiLift(s, f)

  def getOption(s: S): Option[A] = asOptional.getOption(s)

  def set(s: S, b: B): T = asSetter.set(s, b)
  def setF(b: B): S => T = asSetter.setF(b)
  def setOption(from: S, newValue: B): Option[T] = asOptional.setOption(from, newValue)
  def setOptionF(b: B): S => Option[T] = asOptional.setOptionF(b)

  def modify(s: S, f: A => B): T = asSetter.modify(s, f)
  def modifyF(f: A => B): S => T = asSetter.modifyF(f)
  def modifyOption(from: S, f: A => B): Option[T] = asOptional.modifyOption(from, f)
  def modifyOptionF(f: A => B): S => Option[T] = asOptional.modifyOptionF(f)


  lazy val asOptional = new Optional[S, T, A, B]{
    def multiLift[F[_]: Applicative](from: S, f: A => F[B]): F[T] =
      seta(from) // T \/ A
        .map(f) // T \/ F[B]
        .map(Applicative[F].map(_)(_reverseGet)) // T \/ F[T]
        .leftMap(Applicative[F].point(_)) // F[T] \/ F[T]
        .fold(identity, identity) // F[T]
  }

  def asTraversal = asOptional.asTraversal
  def asSetter    = asOptional.asSetter
  def asFold      = asOptional.asFold

  def composePrism[C, D](other: Prism[A, B, C, D]): Prism[S, T, C, D] =
    new Prism[S, T, C, D](
      reverseGet _ compose other.reverseGet,
      s => seta(s) flatMap( a => other.seta(a) leftMap _reverseGet )
    )

  def composeIso[C, D](other: Iso[A, B, C, D])            : Prism[S, T, C, D]     = composePrism(other.asPrism)

  def composeLens[C, D](other: Lens[A, B, C, D])          : Optional[S, T, C, D]  = asOptional  composeOptional  other.asOptional
  def composeOptional[C, D](other: Optional[A, B, C, D])  : Optional[S, T, C, D]  = asOptional  composeOptional  other
  def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = asTraversal composeTraversal other
  def composeSetter[C, D](other: Setter[A, B, C, D])      : Setter[S, T, C, D]    = asSetter    composeSetter    other
  def composeFold(other: Fold[A, B])                      : Fold[S, B]            = asFold      composeFold      other

}

object Prism {

  def apply[S, T, A, B](_reverseGet: B => T, seta: S => T \/ A): Prism[S, T, A, B] =
    new Prism[S, T, A, B](_reverseGet, seta)

}
