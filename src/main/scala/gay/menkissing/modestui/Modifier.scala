package gay.menkissing.modestui

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.syntax.all.*
import shapeless3.deriving.K0
import fs2.concurrent.Signal
trait Modifier[F[_], E, A] { outer =>
  def modify(a: A, e: E): F[Unit]

  inline final def contramap[B](inline f: B => A): Modifier[F, E, B] =
    (b: B, e: E) => outer.modify(f(b), e)
}

object Modifier {
  given forUnit[F[_], E](using F: Applicative[F]): Modifier[F, E, Unit] =
    (_, _) => F.pure(())
  given forTuple[F[_], E, M <: Tuple](
      using inst: K0.ProductInstances[[X] =>> Modifier[F, E, X], M],
      F: Applicative[F]
    ): Modifier[F, E, M] = (m, e) =>
      inst.foldLeft(m)(F.pure(())) {
        [a] => (r: F[Unit], m: Modifier[F, E, a], a: a) => r *> m.modify(a, e)
      }
}
