package monocle

import scalaz.Id._
import scalaz.std.list._
import scalaz.{Applicative, Const, Traverse}

/**
 * A Traversal is generalisation of a Lens in a way that it defines a multi foci between
 * S and 0 to many A.
 */
abstract class Traversal[S, T, A, B] private[monocle] { self =>

  def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] // to define

  final def getAll(s: S): List[A] = asFold.getAll(s)

  final def set(s: S, b: B): T = asSetter.set(s, b)
  final def setF(b: B): S => T = asSetter.setF(b)

  final def modify(s: S, f: A => B): T = asSetter.modify(s, f)
  final def modifyF(f: A => B): S => T = asSetter.modifyF(f)

  final lazy val asSetter: Setter[S, T, A, B] = Setter[S, T, A, B](f => s => multiLift[Id](s, f))
  final lazy val asFold  : Fold[S, A] = Fold { s: S =>
    multiLift[({type l[a] = Const[List[A], a]})#l](s, { a: A => Const[List[A], B](List(a))}).getConst
  }

  final def composeSetter[C, D](other: Setter[A, B, C, D])    : Setter[S, T, C, D]    = asSetter composeSetter other
  final def composeFold(other: Fold[A, B])                    : Fold[S, B]            = asFold   composeFold   other

  final def composeTraversal[C, D](other: Traversal[A, B, C, D]): Traversal[S, T, C, D] = new Traversal[S, T, C, D] {
    def multiLift[F[_]: Applicative](s: S, f: C => F[D]): F[T] = self.multiLift(s, other.multiLift[F](_, f))
  }

  final def composeOptional[C, D](other: Optional[A, B, C, D]): Traversal[S, T, C, D]  = composeTraversal(other.asTraversal)
  final def composePrism[C, D](other: Prism[A, B, C, D])      : Traversal[S, T, C, D]  = composeTraversal(other.asTraversal)
  final def composeLens[C, D](other: Lens[A, B, C, D])        : Traversal[S, T, C, D]  = composeTraversal(other.asTraversal)
  final def composeIso[C, D](other: Iso[A, B, C, D])          : Traversal[S, T, C, D]  = composeTraversal(other.asTraversal)

}

object Traversal {

  def apply[T[_]: Traverse, A, B]: Traversal[T[A], T[B], A, B] = new Traversal[T[A], T[B], A, B] {
    def multiLift[F[_]: Applicative](s: T[A], f: A => F[B]): F[T[B]] = Traverse[T].traverse(s)(f)
  }

  def apply2[S, T, A, B](get1: S => A, get2: S => A)(_set: (S, B, B) => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] =
      Applicative[F].apply2(f(get1(s)), f(get2(s)))((v1, v2) => _set(s, v1, v2))
  }

  def apply3[S, T, A, B](get1: S => A, get2: S => A, get3: S => A)(_set: (S, B, B, B) => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] =
      Applicative[F].apply3(f(get1(s)), f(get2(s)), f(get3(s)))((v1, v2, v3) => _set(s, v1, v2, v3))
  }

  def apply4[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A)(_set: (S, B, B, B, B) => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] =
      Applicative[F].apply4(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)))((v1, v2, v3, v4) => _set(s, v1, v2, v3, v4))
  }

  def apply5[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A)(_set: (S, B, B, B, B, B) => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] =
      Applicative[F].apply5(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)))((v1, v2, v3, v4, v5) => _set(s, v1, v2, v3, v4, v5))
  }

  def apply6[S, T, A, B](get1: S => A, get2: S => A, get3: S => A, get4: S => A, get5: S => A, get6: S => A)(_set: (S, B, B, B, B, B, B) => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def multiLift[F[_]: Applicative](s: S, f: A => F[B]): F[T] =
      Applicative[F].apply6(f(get1(s)), f(get2(s)), f(get3(s)), f(get4(s)), f(get5(s)), f(get6(s)))((v1, v2, v3, v4, v5, v6) => _set(s, v1, v2, v3, v4, v5, v6))
  }

}
