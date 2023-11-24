package gay.menkissing.modestui.ui

import io.github.humbleui.skija.{Canvas, Font, FontMetrics, Paint, TextLine}
import io.github.humbleui.skija.shaper.ShapingOptions
import gay.menkissing.modestui.core.*
import io.github.humbleui.types.{IPoint, IRect}
import cats.effect.* 
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
class Label(val paint: Paint, val line: TextLine, val metrics: FontMetrics)

given [F[_]](using F: Sync[F], M: Monad[F]): ATerminal[F, Label] with
  extension (self: Label) {
    def measure(p: IPoint): F[IPoint] = 
      // metrics is pure : )
      for {
        width <- F.delay { self.line.getWidth }
        capHeight = self.metrics.getCapHeight
      } yield IPoint(width.toInt, capHeight.toInt)
    def draw(rect: IRect, canvas: Canvas): F[Unit] = {
      // Rect is immutable. This means access to it is pure
        // TODO: why does original call capHeight?
       F.delay { canvas.drawTextLine(self.line, rect._left, rect._top, self.paint) }
    }
  }
