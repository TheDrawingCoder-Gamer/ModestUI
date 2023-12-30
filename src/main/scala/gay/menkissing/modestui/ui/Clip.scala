package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*
import fs2.concurrent.Topic

import gay.menkissing.modestui.*

import io.github.humbleui.skija.Canvas
import io.github.humbleui.jwm.Event
import io.github.humbleui.types.{RRect, IRect}

class Clip[F[_], C](val myChild: C)

object Clip {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[C](child: C)(using C: Component[F, C], F: Async[F]) =
      new Clip[F, C](child)
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
class RRectClip[F[_], C](val radius: Float, val myChild: C)

object RRectClip {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[C](radius: Float, child: C)(using C: Component[F, C], F: Async[F]) =
        new RRectClip[F, C](radius, child)
  }
  def apply[F[_]] = new BuildOps[F]
}

given rrectClipComponent[F[_], C](using F: Async[F], C: Component[F, C]): AWrapper[F, RRectClip[F, C], C] with
  extension (self: RRectClip[F, C]) {
    def child = F.pure(self.myChild)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = 
      canvas.ioBracket.use { canvas =>
        for {
          _ <- F.delay { canvas.clipRRect(RRect.makeXYWH(rect.getLeft, rect.getTop, rect.getWidth, rect.getHeight, (ctx.scale * self.radius).toFloat)) }
          _ <- self.myChild.draw(ctx, rect, canvas)
        } yield ()
      }
  }
