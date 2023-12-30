package gay.menkissing.modestui.ui

import io.github.humbleui.skija.{Canvas, Font, FontMetrics, Paint, TextLine, BreakIterator}
import io.github.humbleui.skija.shaper.{ShapingOptions, Shaper}
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.*
import io.github.humbleui.types.{IPoint, IRect, Point}
import cats.effect.* 
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import scala.util.chaining.*


case class Token(text: String, shaped: TextLine, blank: Boolean)
case class Layout(positions: List[Option[Point]], width: Float, height: Float)

def layout(tokens: List[Token], maxWidth: Float, capHeight: Float, lineHeight: Float) = {
  val positions = collection.mutable.ListBuffer[Option[Point]]()
  var x = 0.0f
  var y = 0.0f
  var width = 0.0f
  var height = 0.0f
  for (token <- tokens) {
    val shaped = token.shaped
    val blank = token.blank
    val tokenWidth = shaped.getWidth
    if (x == 0 && y == 0) {
      positions.prepend(Option(Point(0, capHeight)))
      x = tokenWidth
      y = capHeight
      width = math.max(width, tokenWidth)
      height = capHeight
    } else if (blank) {
      positions.prepend(None)
      x += tokenWidth
      // y
      // width
      // height
    } else if (x + tokenWidth <= maxWidth) {
      positions.prepend(Some(Point(x, y)))
      x += tokenWidth
      // y
      width = math.max(width, x + tokenWidth)
      // height
    } else {
      positions.prepend(Some(Point(0, y + lineHeight)))
      x = tokenWidth
      y += lineHeight
      width = math.max(width, tokenWidth)
      height += lineHeight
    }
  }
  Layout(positions.toList.reverse, width, height)

}

def splitWhitespace(s: String): List[String] = {
  val trimmed = s.stripTrailing()
  val space = s.substring(trimmed.length)
  if (space.length > 0)
    List(trimmed, space)
  else
    List(s)
}
def words(text: String): List[String] = {
  val iter = BreakIterator.makeLineInstance(text)
  @annotation.tailrec
  def loop(words: List[String], start: Int): List[String] = {
    val end = iter.next()
    if (end == BreakIterator.DONE) {
      words.appended(text.substring(start))
    } else {
      loop(words.appendedAll(splitWhitespace(text.substring(start, end))), end)
    }
  }
  val res = loop(List(), 0)
  iter.close()
  res
}
class Paragraph[F[_]](val paint: Paint, val tokens: List[Token], val lineHeight: Float, val metrics: FontMetrics)


object Paragraph {
  case class Opts(font: Option[Font] = None, paint: Option[Paint] = None, lineHeight: Option[Float] = None, features: List[String] = List())
  def apply[F[_]](text: String, opts: Opts = Opts())(using F: Async[F]) = {
    Contextual[F] { ctx =>
      val font = opts.font.getOrElse(ctx.fontUi.get)
      val paint = opts.paint.getOrElse(ctx.fillText.get)
      val lineHeight = math.ceil(opts.lineHeight.map(_ * ctx.scale).getOrElse(2 * font.getMetrics.getCapHeight)).toInt
      val features = ShapingOptions.DEFAULT.tap { it =>
        if (!opts.features.isEmpty)
          it.withFeatures(opts.features.mkString(" "))
      }
      // this is pure ENOUGH:tm:
      val tokens = words(text).map(it => Token(it, Shaper.makeShapeDontWrapOrReorder().shapeLine(it, font, features), it.trim().isEmpty))
      F.delay { new Paragraph[F](paint, tokens, lineHeight, font.getMetrics) }.toResource
    } 
  }
}

given component_Paragraph[F[_]](using F: Async[F]): ATerminal[F, Paragraph[F]] with
  extension (self: Paragraph[F]) {
    def measure(ctx: Context, p: IPoint): F[IPoint] = {
      val dalayout = layout(self.tokens, p.getX, self.metrics.getCapHeight, self.lineHeight)
      F.pure { IPoint(math.ceil(dalayout.width).toInt, dalayout.height.toInt) }
    }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      val dalayout = layout(self.tokens, rect.getWidth, self.metrics.getCapHeight, self.lineHeight)
      dalayout.positions.zip(self.tokens).traverse_ {
        case (Some(pos), token) =>
          F.delay {
            canvas.drawTextLine(token.shaped, rect.getLeft + pos.getX, rect.getTop + pos.getY, self.paint)
          }.void
        case _ => 
          F.unit
      }
    }
  }
