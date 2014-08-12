package monocle.function

import monocle.{Optional, SimpleOptional}
import monocle.std._

import scala.annotation.implicitNotFound
import scalaz.syntax.traverse._
import scalaz.{Applicative, Traverse}

@implicitNotFound("Could not find an instance of Index[${S},${I},${A}], please check Monocle instance location policy to " +
  "find out which import is necessary")
trait Index[S, I, A] {

  /**
   * Creates a Traversal from S to 0 or 1 A
   * index is less powerful than at as we can only modify an index that already exist
   */
  def index(i: I): SimpleOptional[S, A]

}

object Index extends IndexFunctions

trait IndexFunctions {

  def index[S, I, A](i: I)(implicit ev: Index[S, I, A]): SimpleOptional[S, A] = ev.index(i)

  def atIndex[S, I, A](implicit ev: At[S, I, A]) = new Index[S, I, A] {
    def index(i: I) = ev.at(i) composePrism some
  }

  def traverseIndex[S[_]: Traverse, A](zipWithIndex: S[A] => S[(A, Int)]): Index[S[A], Int, A] = new Index[S[A], Int, A]{
    def index(i: Int) = new Optional[S[A], S[A], A, A] {
      def multiLift[F[_] : Applicative](from: S[A], f: A => F[A]): F[S[A]] =
        zipWithIndex(from).traverse { case (a, j) =>
          if(j == i) f(a) else Applicative[F].point(a)
        }
    }
  }

}


