package monocle


import monocle.function.Each._
import monocle.std.option._
import org.specs2.execute.AnyValueAsResult
import org.specs2.scalaz.Spec
import shapeless.test.illTyped

/**
 * Illustrate the purpose of specific compose function for each main concept (Lens, Traversal, etc)
 */
class ComposeIssueExample extends Spec {

  case class Example(_opt: Option[Int])

  val optLens = Macro.mkLens[Example, Option[Int]]("_opt")
  val example = Example(Some(2))

  "compose does not compile between Lens and Prism" in {
    new AnyValueAsResult[Unit].asResult(illTyped("""
      optLens.compose(some).getAll(example)
    """))
  }

  "but composeTraversal does" in {
    optLens.compose(some[Int, Int]).getOption(example) shouldEqual Some(2)
  }

  "also when type parameter is explicit" in {
    (optLens.compose(some: SimplePrism[Option[Int], Int])).getOption(example) shouldEqual Some(2)
  }

  "compose and implicit do not work together" in {
    new AnyValueAsResult[Unit].asResult(illTyped("""
      optLens.compose(each).getAll(example)
    """))
  }

  "but composeTraversal is fine" in {
    optLens.compose(each[Option[Int], Int]).getAll(example) shouldEqual List(2)
  }


}
