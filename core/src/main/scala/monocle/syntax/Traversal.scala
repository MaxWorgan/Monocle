package monocle.syntax

import monocle._
import scalaz.Applicative

object traversal extends TraversalSyntax

private[syntax] trait TraversalSyntax {
  implicit def toApplyTraversalOps[S](value: S): ApplyTraversalOps[S] = ApplyTraversalOps(value)
}

final case class ApplyTraversalOps[S](s: S) {
  def applyTraversal[T, A, B](traversal: Traversal[S, T, A, B]): ApplyTraversal[S, T, A, B] =
    new ApplyTraversal[S, T, A, B](s, traversal)
}

final case class ApplyTraversal[S, T, A, B](s: S, traversal: Traversal[S, T, A, B]) {

  def multiLift[F[_]: Applicative](f: A => F[B]): F[T] = traversal.multiLift[F](s, f)

  def set(s: S, b: B): T = traversal.set(s, b)
  def setF(b: B): S => T = traversal.setF(b)

  def modify(s: S, f: A => B): T = traversal.modify(s, f)
  def modifyF(f: A => B): S => T = traversal.modifyF(f)

  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] = ApplySetter(s, traversal composeSetter other)
  def composeFold(other: Fold[A, B]): ApplyFold[S, B] = ApplyFold(s, traversal composeFold  other)

  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplyTraversal[S, T, C, D] =
    new ApplyTraversal[S, T, C, D](s, traversal composeTraversal other)

  def composeOptional[C, D](other: Optional[A, B, C, D]): ApplyTraversal[S, T, C, D] = composeTraversal(other.asTraversal)
  def composePrism[C, D](other: Prism[A, B, C, D])      : ApplyTraversal[S, T, C, D] = composeTraversal(other.asTraversal)
  def composeLens[C, D](other: Lens[A, B, C, D])        : ApplyTraversal[S, T, C, D] = composeTraversal(other.asTraversal)
  def composeIso[C, D](other: Iso[A, B, C, D])          : ApplyTraversal[S, T, C, D] = composeTraversal(other.asTraversal)

}