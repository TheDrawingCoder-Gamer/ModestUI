package gay.menkissing.modestui.ui

import gay.menkissing.modestui.*

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.implicits.*

import io.github.humbleui.skija.Canvas
import io.github.humbleui.types.{IPoint, IRect}

class ACanvas[F[_]](val onPaint: Option[(Context, Canvas, IPoint) => F[Unit]], val onEvent: Option[(Context, events.MEvent) => F[Boolean]], val curRect: Ref[F, IRect])

object ACanvas {
  def apply[F[_]](onPaint: (Context, Canvas, IPoint) => F[Unit] = null, onEvent: (Context, events.MEvent) => F[Boolean] = null)(using F: Sync[F]) =
    for {
      curRect <- Ref[F].of(IRect(0, 0, 0, 0))
    } yield new ACanvas[F](Option(onPaint), Option(onEvent), curRect)
}

given component_ACanvas[F[_]](using F: Sync[F]): ATerminal[F, ACanvas[F]] with {
  extension (self: ACanvas[F]) {
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      for {
        _ <- self.curRect.set(rect)
        _ <- self.onPaint.traverse { onPaint =>
          canvas.ioBracket.use { daCanvas =>
            F.delay { daCanvas.clipRect(rect.toRect) }
            *> F.delay { daCanvas.translate(rect.getLeft, rect.getTop) }
            *> onPaint(ctx, daCanvas, IPoint(rect.getWidth, rect.getHeight))
          }
        }
      } yield ()
    def measure(ctx: Context, p: IPoint): F[IPoint] =
      p.pure[F]
    override def event(ctx: Context, event: events.MEvent): F[Boolean] =
      self.onEvent.traverse { onEvent =>
        for {
          rect <- self.curRect.get
          newEvent =
            event match {
              case e: events.MMouseEvent =>
                e.withPoint(IPoint(e.x - rect.getLeft, e.y - rect.getTop))
              case e =>
                e
            }
          res <- onEvent(ctx, newEvent)
        } yield res
      }.map(_.getOrElse(false))
  }
}


