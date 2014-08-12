package monocle.syntax

import monocle._

import scalaz.Functor

object lens extends LensSyntax

private[syntax] trait LensSyntax {
  implicit def toApplyLensOps[S](value: S): ApplyLensOps[S] = new ApplyLensOps(value)
}

final case class ApplyLens[S, T, A, B](s: S, lens: Lens[S, T, A, B]) {

  def lift[F[_]: Functor](f: A => F[B]): F[T] = lens.lift[F](s, f)

  def get: A = lens.get(s)

  def set(s: S, newValue: B): T  = lens.set(s, newValue)
  def setF(b: B): S => T = lens.setF(b)

  def modify(s: S, f: A => B): T = lens.modify(s, f)
  def modifyF(f: A => B): S => T = lens.modifyF(f)

  def composeOptional[C, D](other: Optional[A, B, C, D]): ApplyOptional[S, T, C, D] =
    ApplyOptional(s, lens composeOptional other)
  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplyTraversal[S, T, C, D] =
    ApplyTraversal(s, lens composeTraversal other)
  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] =
    ApplySetter(s, lens composeSetter other)
  def composeGetter(other: Getter[A, B]): ApplyGetter[S, B] =
    ApplyGetter(s, lens composeGetter other)
  def composeFold(other: Fold[A, B]): ApplyFold[S, B] =
    ApplyFold(s, lens composeFold other)
  def composePrism[C, D](other: Prism[A, B, C, D]): ApplyOptional[S, T, C, D] =
    ApplyOptional(s, lens composePrism other)

  def composeLens[C, D](other: Lens[A, B, C, D]): ApplyLens[S, T, C, D] =
    new ApplyLens[S, T, C, D](s, lens composeLens other)

  def composeIso[C, D](other: Iso[A, B, C, D]): ApplyLens[S, T, C, D] = composeLens(other.asLens)

}

private[syntax] final class ApplyLensOps[S](s: S) {
  def applyLens[T, A, B](lens: Lens[S, T, A, B]): ApplyLens[S, T, A, B] = 
    new ApplyLens[S, T, A, B](s, lens)
}