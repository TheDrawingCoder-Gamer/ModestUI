package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*
import fs2.concurrent.Topic

import gay.menkissing.modestui.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.Component

import io.github.humbleui.skija.Canvas
import io.github.humbleui.jwm.Event
import io.github.humbleui.types.IRect

// TODO: Some things don't really make sense to have a topic, but it's easier this way to prevent sadness
class Clip[F[_], C](val myChild: C, topic: Topic[F, Event]) extends HasTopic[F](topic)

object Clip {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[C](child: C)(using C: Component[F, C], F: Async[F]) =
      for {
        topic <- Topic[F, Event].toResource
        clip <- F.delay { new Clip(child, topic) }.toResource
      } yield clip
  }
  def apply[F[_]] = new BuildOps[F]
}

given clipComponent[F[_], C](using F: Async[F], C: Component[F, C]): AWrapper[F, Clip[F, C], C] with
  extension (self: Clip[F, C]) {
    def child = F.pure(self.myChild)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = 
      canvas.ioBracket.use { canvas =>
        for {
          _ <- F.delay { canvas.clipRect(rect.toRect) }
          _ <- self.myChild.draw(ctx, rect, canvas)
        } yield ()
      }
  }
