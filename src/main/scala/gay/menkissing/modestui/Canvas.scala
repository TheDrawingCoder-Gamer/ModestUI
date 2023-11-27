package gay.menkissing.modestui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*
import io.github.humbleui.skija.Canvas

extension (self: Canvas) {
  def ioBracket[F[_]](using F: Async[F]): Resource[F, Canvas] =
    for {
      count <- F.delay { self.save() }.toResource
      canvas <- Resource.make[F, Canvas](F.pure(self))(it => F.delay { it.restoreToCount(count) })
    } yield canvas
}
