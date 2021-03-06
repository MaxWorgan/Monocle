package monocle.function

import monocle.SimplePrism
import monocle.internal.Bounded

import scala.annotation.implicitNotFound
import scalaz.syntax.order._
import scalaz.{Maybe, Order}

@implicitNotFound("Could not find an instance of SafeCast[${S},${A}], please check Monocle instance location policy to " +
  "find out which import is necessary")
trait SafeCast[S, A] {
  
  def safeCast: SimplePrism[S, A]

}

object SafeCast extends SafeCastFunctions

trait SafeCastFunctions {
  
  def safeCast[S, A](implicit ev: SafeCast[S, A]): SimplePrism[S, A] = ev.safeCast

  def orderingBoundedSafeCast[S: Order, A: Bounded](unsafeCast: S => A)(revCast: A => S): SafeCast[S, A] = new SafeCast[S, A] {
    def safeCast = SimplePrism[S, A]( from =>
      if (from > revCast(Bounded[A].MaxValue) || from < revCast(Bounded[A].MinValue))
        Maybe.empty
      else
        Maybe.just(unsafeCast(from))
    )(revCast)
  }

}

