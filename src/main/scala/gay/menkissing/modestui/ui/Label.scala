package gay.menkissing.modestui.ui

import io.github.humbleui.skija.{Canvas, Font, FontMetrics, Paint, TextLine}
import io.github.humbleui.skija.shaper.{ShapingOptions, Shaper}
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.*
import io.github.humbleui.types.{IPoint, IRect}
import cats.effect.* 
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import fs2.concurrent.Topic
class Label[F[_]] private (val paint: Paint, val line: TextLine, val metrics: FontMetrics, topic: Topic[F, Event]) extends HasTopic[F](topic)
object Label {
    def apply[F[_]](text: String)(using F: Async[F]): Resource[F, Label[F]] = {
      for {
        font <- F.delay { Font() }.toResource
        paint <- F.delay { Paint() }.toResource
        line <- F.delay { Shaper.makeShapeDontWrapOrReorder().shapeLine(text, font, ShapingOptions.DEFAULT) }.toResource
        metrics <- F.delay { font.getMetrics() }.toResource
        topic <- Topic[F, Event].toResource
        label <- F.delay { new Label(paint, line, metrics, topic) }.toResource
      } yield label
    }
  }
given [F[_]](using F: Async[F], M: Monad[F]): ATerminal[F, Label[F]] with
  extension (self: Label[F]) {
    def measure(ctx: Context, p: IPoint): F[IPoint] = 
      // metrics is pure : )
      for {
        width <- F.delay { self.line.getWidth }
        capHeight = self.metrics.getCapHeight
      } yield IPoint(width.toInt, capHeight.toInt)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      // Rect is immutable. This means access to it is pure
        // TODO: why does original call capHeight?
       F.delay { canvas.drawTextLine(self.line, rect._left, rect._top + math.ceil(self.metrics.getCapHeight).toInt, self.paint) }
    }
  }

  
