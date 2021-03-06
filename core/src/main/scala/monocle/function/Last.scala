package monocle.function

import monocle.SimpleLens
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Last[${S},${A}], please check Monocle instance location policy to " +
  "find out which import is necessary")
trait Last[S, A] {

  /**
   * Creates a Lens from S to its last element
   * last is safe, it should only be implemented on object with a last element
   */
  def last: SimpleLens[S, A]

}


object Last extends LastFunctions

trait LastFunctions {

  def last[S, A](implicit ev: Last[S, A]): SimpleLens[S, A] = ev.last

}