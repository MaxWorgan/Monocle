package monocle.syntax

import monocle.Fold
import scalaz.Monoid

object fold extends FoldSyntax

private[syntax] trait FoldSyntax {
  implicit def toApplyFoldOps[S](value: S): ApplyFoldOps[S] = new ApplyFoldOps(value)
}


final case class ApplyFold[S, A](s: S, _fold: Fold[S, A]) {

  def foldMap[B: Monoid](f: A => B): B = _fold.foldMap(s)(f)

  def fold(implicit ev: Monoid[A]): A = _fold.fold(s)

  def getAll: List[A] = _fold.getAll(s)

  def headOption: Option[A] = _fold.headOption(s)

  def exist(p: A => Boolean): Boolean = _fold.exist(s)(p)

  def all(p: A => Boolean): Boolean = _fold.all(s)(p)

  def composeFold[B](other: Fold[A, B]): ApplyFold[S, B] =
    new ApplyFold[S, B](s, _fold composeFold other)
}

private[syntax] final class ApplyFoldOps[S](s: S) {
  def applyFold[A](fold: Fold[S, A]): ApplyFold[S, A] =
    new ApplyFold[S, A](s, fold)
}