package monocle.syntax

import monocle._

import scalaz.Functor

object iso extends IsoSyntax

private[syntax] trait IsoSyntax {
  implicit def toApplyIsoOps[S](value: S): ApplyIsoOps[S] = new ApplyIsoOps(value)
}


final case class ApplyIso[S, T, A, B](s: S, iso: Iso[S, T, A, B]) {

  def lift[F[_]: Functor](f: A => F[B]): F[T] = iso.lift[F](s, f)

  def get: A = iso.get(s)

  def set(s: S, newValue: B): T  = iso.set(s, newValue)
  def setF(b: B): S => T = iso.setF(b)

  def modify(s: S, f: A => B): T = iso.modify(s, f)
  def modifyF(f: A => B): S => T = iso.modifyF(f)


  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] =
    ApplySetter(s, iso composeSetter other)
  def composeGetter(other: Getter[A, B]): ApplyGetter[S, B] =
    ApplyGetter(s, iso composeGetter other)
  def composeFold(other: Fold[A, B]): ApplyFold[S, B] =
    ApplyFold(s, iso composeFold other)
  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplyTraversal[S, T, C, D] =
    ApplyTraversal(s, iso composeTraversal other)
  def composeOptional[C, D](other: Optional[A, B, C, D]): ApplyOptional[S, T, C, D] =
    ApplyOptional(s, iso composeOptional other)
  def composePrism[C, D](other: Prism[A, B, C, D]): ApplyPrism[S, T, C, D] =
    ApplyPrism(s, iso composePrism other)
  def composeLens[C, D](other: Lens[A, B, C, D]): ApplyLens[S, T, C, D] =
    ApplyLens[S, T, C, D](s, iso composeLens other)
  def composeIso[C, D](other: Iso[A, B, C, D]): ApplyIso[S, T, C, D] = 
    ApplyIso[S, T, C, D](s, iso composeIso other)

}

final case class ApplyIsoOps[S](s: S) {
  def applyIso[T, A, B](iso: Iso[S, T, A, B]): ApplyIso[S, T, A, B] =
    ApplyIso[S, T, A, B](s, iso)
}
