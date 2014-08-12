package monocle

import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.{ Foldable, Monoid, Tag }

sealed abstract class Fold[S, A] private { self =>

  def foldMap[B: Monoid](s: S)(f: A => B): B // to define


  final def fold(from: S)(implicit ev: Monoid[A]): A = foldMap(from)(identity)

  final def getAll(from: S): List[A] = foldMap(from)(List(_))

  final def headOption(from: S): Option[A] = Tag.unwrap(foldMap(from)(Option(_).first))

  final def exist(from: S)(p: A => Boolean): Boolean = Tag.unwrap(foldMap(from)(p(_).disjunction))

  final def all(from: S)(p: A => Boolean): Boolean = Tag.unwrap(foldMap(from)(p(_).conjunction))

  final def composeFold[B](other: Fold[A, B]): Fold[S, B] = new Fold[S, B] {
    def foldMap[C: Monoid](from: S)(f: B => C): C = self.foldMap(from)(other.foldMap(_)(f))
  }

  final def composeGetter[B](other: Getter[A, B])        : Fold[S, B] = composeFold(other.asFold)
  final def composeLens[B, C, D](other: Lens[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
  final def composeIso[B, C, D](other: Iso[A, B, C, D])  : Fold[S, C] = composeFold(other.asFold)

}

object Fold {

  def apply[S, A](_get: S => A): Fold[S, A] = new Fold[S, A]{
    def foldMap[B: Monoid](s: S)(f: A => B): B = (f compose _get)(s)
  }

  def apply[F[_]: Foldable, S, A](_getAll: S => F[A]): Fold[S, A] = new Fold[S, A]{
    def foldMap[B: Monoid](s: S)(f: A => B): B = Foldable[F].foldMap(_getAll(s))(f)
  }

  def apply[F[_]: Foldable, A]: Fold[F[A], A] = apply[F, F[A], A](identity)

}
