package monocle.syntax

import monocle._

object prism extends PrismSyntax

private[syntax] trait PrismSyntax {
  implicit def toApplyPrismOps[S](value: S): ApplyPrismOps[S] = new ApplyPrismOps(value)
}


final case class ApplyPrism[S, T, A, B](s: S, prism: Prism[S, T, A, B]){

  def getOption: Option[A] = prism.getOption(s)

  def set(s: S, newValue: B): T  = prism.set(s, newValue)
  def setF(b: B): S => T = prism.setF(b)
  def setOption(s: S, newValue: B): Option[T] = prism.modifyOption(s, _ => newValue)
  def setOptionF(newValue: B): S => Option[T] = prism.setOptionF(newValue)

  def modify(s: S, f: A => B): T = prism.modify(s, f)
  def modifyF(f: A => B): S => T = prism.modifyF(f)
  def modifyOption(s: S, f: A => B): Option[T] = prism.modifyOption(s, f)
  def modifyOptionF(f: A => B): S => Option[T] = prism.modifyOptionF(f)


  def composeSetter[C, D](other: Setter[A, B, C, D]): ApplySetter[S, T, C, D] =
    ApplySetter(s, prism composeSetter other)
  def composeFold(other: Fold[A, B]): ApplyFold[S, B] =
    ApplyFold(s, prism composeFold other)
  def composeTraversal[C, D](other: Traversal[A, B, C, D]): ApplyTraversal[S, T, C, D] =
    ApplyTraversal(s, prism composeTraversal other)
  def composeOptional[C, D](other: Optional[A, B, C, D]): ApplyOptional[S, T, C, D] =
    new ApplyOptional[S, T, C, D](s, prism composeOptional other)

  def composePrism[C, D](other: Prism[A, B, C, D]): ApplyPrism[S, T, C, D] =
    new ApplyPrism[S, T, C, D](s, prism composePrism other)

  def composeIso[C, D](other: Iso[A, B, C, D]): ApplyPrism[S, T, C, D] =
    composePrism(other.asPrism)

}

final case class ApplyPrismOps[S](s: S) {
  def applyPrism[T, A, B](prism: Prism[S, T, A, B]): ApplyPrism[S, T, A, B] =
    new ApplyPrism[S, T, A, B](s, prism)
}
