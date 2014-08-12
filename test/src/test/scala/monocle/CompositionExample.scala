package monocle

import org.specs2.scalaz.Spec


class CompositionExample extends Spec {

  case class Order(articles: List[Article])
  case class Article(name: String)

  val _articles = SimpleLens[Order](_.articles)((o, a) => o.copy(articles = a))
  val _name = SimpleLens[Article](_.name)((a, n) => a.copy(name = n))

  "compose" in {
    import Monocle._
    import Composition.instances._
    import Composition.syntax._

    val order = Order(List(Article("fruit"), Article("meat")))

    (_articles <> headOption <> _name).getOption(order) shouldEqual Some("fruit")
  }

}
