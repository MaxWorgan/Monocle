package monocle

final class Getter[S, A] private (_get: S => A) {

  def get(from: S): A = _get(from)

  lazy val asFold = Fold(_get)

  def composeGetter[B](other: Getter[A, B]): Getter[S, B] = 
    new Getter[S, B](other.get _ compose get)

  def composeFold[B](other: Fold[A, B]): Fold[S, B] = asFold composeFold other

  def composeLens[B, C, D](other: Lens[A, B, C, D]): Getter[S, C] = composeGetter(other.asGetter)
  def composeIso[B, C, D](other: Iso[A, B, C, D])  : Getter[S, C] = composeGetter(other.asGetter)

}

object Getter {
  def apply[S, A](_get: S => A): Getter[S, A] = new Getter[S, A](_get)
}
