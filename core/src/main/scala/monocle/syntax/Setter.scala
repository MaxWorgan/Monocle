package monocle.syntax

import monocle._

object setter extends SetterSyntax

private[syntax] trait SetterSyntax {
  implicit def toApplySetterOps[S](value: S): ApplySetterOps[S] = ApplySetterOps(value)
}

final case class ApplySetter[S, T, A, B](s: S, setter: Setter[S, T, A, B]){

  def set(s: S, b: B): T = setter.set(s, b)
  def setF(b: B): S => T = setter.setF(b)

  def modify(s: S, f: A => B): T = setter.modify(s, f)
  def modifyF(f: A => B): S => T = setter.modifyF(f)

  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] =
    new ApplySetter[S, T, C, D](s, setter composeSetter other)

  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplySetter[S, T, C, D] = composeSetter(other.asSetter)
  def composeOptional[C, D](other: Optional[A, B, C, D])  : ApplySetter[S, T, C, D] = composeSetter(other.asSetter)
  def composePrism[C, D](other: Prism[A, B, C, D])        : ApplySetter[S, T, C, D] = composeSetter(other.asSetter)
  def composeLens[C, D](other: Lens[A, B, C, D])          : ApplySetter[S, T, C, D] = composeSetter(other.asSetter)
  def composeIso[C, D](other: Iso[A, B, C, D])            : ApplySetter[S, T, C, D] = composeSetter(other.asSetter)
}

final case class ApplySetterOps[S](s: S) {
  def applySetter[T, A, B](setter: Setter[S, T, A, B]): ApplySetter[S, T, A, B] =
    ApplySetter[S, T, A, B](s, setter)
}
