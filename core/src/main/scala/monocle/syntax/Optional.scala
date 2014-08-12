package monocle.syntax

import monocle._

object optional extends OptionalSyntax

private[syntax] trait OptionalSyntax {
  implicit def tolApplyOptionalOps[S](value: S): ApplyOptionalOps[S] = new ApplyOptionalOps(value)
}

final case class ApplyOptionalOps[S](s: S) {
  def applyOptional[T, A, B](optional: Optional[S, T, A, B]): ApplyOptional[S, T, A, B] =
    new ApplyOptional[S, T, A, B](s, optional)
}

final case class ApplyOptional[S, T, A, B](s: S, optional: Optional[S, T, A, B]){

  def getOption: Option[A] = optional.getOption(s)

  def set(s: S, newValue: B): T  = optional.set(s, newValue)
  def setF(b: B): S => T = optional.setF(b)
  def setOption(s: S, newValue: B): Option[T] = optional.modifyOption(s, _ => newValue)
  def setOptionF(newValue: B): S => Option[T] = optional.setOptionF(newValue)

  def modify(s: S, f: A => B): T = optional.modify(s, f)
  def modifyF(f: A => B): S => T = optional.modifyF(f)
  def modifyOption(s: S, f: A => B): Option[T] = optional.modifyOption(s, f)
  def modifyOptionF(f: A => B): S => Option[T] = optional.modifyOptionF(f)

  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplyTraversal[S, T, C, D] =
    ApplyTraversal(s, optional composeTraversal other)
  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] =
    ApplySetter(s, optional composeSetter other)
  def composeFold(other: Fold[A, B]): ApplyFold[S, B] =
    ApplyFold(s, optional composeFold other)

  def composeOptional[C, D](other: Optional[A, B, C, D]): ApplyOptional[S, T, C, D] =
    new ApplyOptional[S, T, C, D](s, optional composeOptional other)

  def composeLens[C, D](other: Lens[A, B, C, D])  : ApplyOptional[S, T, C, D] = composeOptional(other.asOptional)
  def composePrism[C, D](other: Prism[A, B, C, D]): ApplyOptional[S, T, C, D] = composeOptional(other.asOptional)
  def composeIso[C, D](other: Iso[A, B, C, D])    : ApplyOptional[S, T, C, D] = composeOptional(other.asOptional)

}

