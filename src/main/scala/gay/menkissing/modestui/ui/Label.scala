package gay.menkissing.modestui.ui

import io.github.humbleui.skija.{Canvas, Font, FontMetrics, Paint, TextLine}
import io.github.humbleui.skija.shaper.{ShapingOptions, Shaper}
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.*
import io.github.humbleui.types.{IPoint, IRect}
import cats.effect.* 
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import fs2.concurrent.Topic
import scala.util.chaining.*

class Label[F[_]] private (val paint: Paint, val line: TextLine, val metrics: FontMetrics)
object Label {
    def apply[F[_]](text: String, font: Font = null, paint: Paint = null, features: List[String] = List())(using F: Async[F]) = {
      Contextual[F] { context =>
        val daFont = Option(font).getOrElse(context.fontUi.get)
        val daPaint = Option(paint).getOrElse(context.fillText.get)
        val daFeatures = ShapingOptions.DEFAULT.tap { it =>
          if (!features.isEmpty)
            it.withFeatures(features.mkString(" "))
        }
        val metrics = daFont.getMetrics
        F.delay { Shaper.makeShapeDontWrapOrReorder().shapeLine(text, daFont, daFeatures) }.toResource.flatMap { line =>
          F.delay { new Label(daPaint, line, metrics) }.toResource
        }
      }
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

  
