package monocle.std

import monocle.function._
import monocle.{ SimplePrism, Prism, Iso }
import scalaz.{ -\/, \/- }

object option extends OptionInstances

trait OptionFunctions {

  def some[A, B]: Prism[Option[A], Option[B], A, B] =
    Prism[Option[A], Option[B], A, B](Some.apply, _.map(\/-(_)) getOrElse -\/(None))

  def none[A]: SimplePrism[Option[A], Unit] =
    SimplePrism[Option[A], Unit](_ => None, { opt => if (opt == None) Some(()) else None })

  def someIso[A, B]: Iso[Some[A], Some[B], A, B] =
    Iso[Some[A], Some[B], A, B](_.get, Some(_))

}

trait OptionInstances extends OptionFunctions {

  implicit def optEach[A]: Each[Option[A], A] = new Each[Option[A], A] {
    def each = some.asOptional.asTraversal
  }

  implicit def optionHeadOption[A]: HeadOption[Option[A], A] = new HeadOption[Option[A], A] {
    def headOption = some.asOptional
  }

  implicit def optionLastOption[A] = new LastOption[Option[A], A] {
    def lastOption = some.asOptional
  }

  implicit def someEach[A]: Each[Some[A], A] = new Each[Some[A], A] {
    def each = someIso.asPrism.asOptional.asTraversal
  }

  implicit def someHeadOption[A]: HeadOption[Some[A], A] = new HeadOption[Some[A], A] {
    def headOption = someIso.asPrism.asOptional
  }

  implicit def someLastOption[A] = new LastOption[Some[A], A] {
    def lastOption = someIso.asPrism.asOptional
  }

}

