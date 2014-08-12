package monocle

trait Composition[A, B, C] {
  def compose(a: A, b: B): C
}

object Composition {

  def apply[A, B, C](f: A => B => C): Composition[A, B, C] = new Composition[A, B, C] {
    def compose(a: A, b: B) = f(a)(b)
  }

  trait Instances0 {

    implicit def GetterWithGetter[S, A, B]: Composition[Getter[S, A], Getter[A, B], Getter[S, B]] =
      Composition[Getter[S, A], Getter[A, B], Getter[S, B]](a => b => a.composeGetter(b))

    implicit def LensWithLens[S, T, A, B, C, D]: Composition[Lens[S, T, A, B], Lens[A, B, C, D], Lens[S, T, C, D]] =
      Composition[Lens[S, T, A, B], Lens[A, B, C, D], Lens[S, T, C, D]](a => b => a.composeLens(b))

    implicit def LensWithOptional[S, T, A, B, C, D]: Composition[Lens[S, T, A, B], Optional[A, B, C, D], Optional[S, T, C, D]] =
      Composition[Lens[S, T, A, B], Optional[A, B, C, D], Optional[S, T, C, D]](a => b => a.composeOptional(b))

    implicit def LensWithTraversal[S, T, A, B, C, D]: Composition[Lens[S, T, A, B], Traversal[A, B, C, D], Traversal[S, T, C, D]] =
      Composition[Lens[S, T, A, B], Traversal[A, B, C, D], Traversal[S, T, C, D]](a => b => a.composeTraversal(b))
  }

  object instances extends Instances0

  class Ops[A](a: A) {
    def <>[B, C](b: B)(implicit c: Composition[A, B, C]): C = c.compose(a, b)
  }

  trait Syntax {
    implicit def ops[A](a: A): Ops[A] = new Ops[A](a)
  }

  object syntax extends Syntax
}