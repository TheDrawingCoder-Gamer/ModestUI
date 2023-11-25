package gay.menkissing.modestui

import cats.effect.*
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import cats.effect.std.Dispatcher
import io.github.humbleui.jwm.{App as JApp}
object App {
  def start[F[_]](eval: Resource[F, Unit])(using F: Async[F]): Resource[F, Unit] =
    for {
      dispatcher <- Dispatcher.sequential[F]
      _ <- F.delay { JApp.start(() => dispatcher.unsafeRunAndForget(eval.useForever)) }.toResource
    } yield ()
}
