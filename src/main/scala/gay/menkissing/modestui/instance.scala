package gay.menkissing.modestui.instance

sealed trait ∃[F[_]] {
  type A
  val fa: F[A]
}

sealed trait Instance[F[_]] extends ∃[[A] =>> (A, F[A])] {
  def item = fa._1
  def instance = fa._2
}

given [F[_], T](using F[T]): Conversion[T, Instance.Aux[F, T]] = Instance.apply
object Instance {
  type Aux[F[_], A0] = Instance[F] { type A = A0 }
  def apply[F[_], A0](daItem: A0)(using F: F[A0]): Aux[F, A0] = 
    new Instance[F] {
      override final type A = A0
      override val fa = (daItem, F)
    }
}
