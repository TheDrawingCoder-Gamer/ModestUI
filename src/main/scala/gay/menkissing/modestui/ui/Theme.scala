package gay.menkissing.modestui.ui

import gay.menkissing.modestui.ui.*
import gay.menkissing.modestui.*

import io.github.humbleui.skija.*

import cats.effect.*
import cats.effect.implicits.*
import cats.* 
import cats.implicits.*


object Theme {
  val faceUi = "face-ui"
  val fontUi = "font-ui"
  val leading = "leading"
  val fillText = "fill-text"
  val fillGray = "fill-gray"

  val buttonBgActive = "button/bg-active"
  val buttonBgHover = "button/bg-hovered"
  val buttonBg = "button/bg"
  val buttonBorderRadius = "button/border-radius"
  val buttonPaddingLeft = "button/padding-left"
  val buttonPaddingTop = "button/padding-top"
  val buttonPaddingRight = "button/padding-right"
  val buttonPaddingBottom = "button/padding-bottom"

  val toggleFillEnabled = "toggle/fill-enabled"
  val toggleFillDisabled = "toggle/fill-disabled"
  val toggleFillHandle = "toggle/fill-handle"
  val toggleFillEnabledActive = "toggle/fill-enabled-active"
  val toggleFillDisabledActive = "toggle/fill-disabled-active"
  val toggleFillHandleActive = "toggle/fill-handle-active"

  val sliderThumbSize = "slider/thumb-size"
  val sliderTrackHeight = "slider/track-height"
  val sliderFillTrackActive = "slider/fill-track-active"
  val sliderFillTrackInactive = "slider/fill-track-inactive"
  val sliderFillThumb = "slider/fill-thumb"
  val sliderStrokeThumb = "slider/stroke-thumb"
  val sliderFillThumbActive = "slider/fill-thumb-active"
  val sliderStrokeThumbActive = "slider/stroke-thumb-active"

  val textFieldCursorBlinkInterval = "text-field/blink-interval"
  val textFieldFillText = "text-field/fill-text"
  val textFieldFillPlaceholder = "text-field/fill-placeholder"
  val textFieldFillCursor = "text-field/fill-cursor"
  val textFieldFillSelectionActive = "text-field/fill-selection-active"
  val textFieldFillSelectionInactive = "text-field/fill-selection-inactive"
  val textFieldFillBgActive = "text-field/fill-bg-active"
  val textFieldFillBgInactive = "text-field/fill-bg-inactive"
  val textFieldBorderRadius = "text-field/border-radius"
  val textFieldBorderActive = "text-field/border-active"
  val textFieldBorderInactive = "text-field/border-inactive"
  val textFieldCursorWidth = "text-field/cursor-width"
  val textFieldPaddingTop = "text-field/padding-top"
  val textFieldPaddingBottom = "text-field/padding-bottom"
  val textFieldPaddingLeft = "text-field/padding-left"
  val textFieldPaddingRight = "text-field/padding-right"

  object exts {
    extension (c: Context) {
      def faceUi = c.getObj[Typeface](Theme.faceUi)
      def fontUi = c.getObj[Font](Theme.fontUi)
      def leading = c.getDouble(Theme.leading)
      def fillText = c.getObj[Paint](Theme.fillText)
      def fillGray = c.getObj[Paint](Theme.fillGray)
      
      def buttonBgActive = c.getObj[Paint](Theme.buttonBgActive)
      def buttonBgHover = c.getObj[Paint](Theme.buttonBgHover)
      def buttonBg = c.getObj[Paint](Theme.buttonBg)
      def buttonBorderRadius = c.getInt(Theme.buttonBorderRadius)
      def buttonPaddingLeft = c.getDouble(Theme.buttonPaddingLeft)
      def buttonPaddingTop = c.getDouble(Theme.buttonPaddingTop)
      def buttonPaddingRight = c.getDouble(Theme.buttonPaddingRight)
      def buttonPaddingBottom = c.getDouble(Theme.buttonPaddingBottom)
      
      def toggleFillEnabled = c.getObj[Paint](Theme.toggleFillEnabled)
      def toggleFillDisabled = c.getObj[Paint](Theme.toggleFillDisabled)
      def toggleFillHandle = c.getObj[Paint](Theme.toggleFillHandle)
      def toggleFillEnabledActive = c.getObj[Paint](Theme.toggleFillEnabledActive)
      def toggleFillDisabledActive = c.getObj[Paint](Theme.toggleFillDisabledActive)
      def toggleFillHandleActive = c.getObj[Paint](Theme.toggleFillHandleActive)

      def sliderThumbSize = c.getDouble(Theme.sliderThumbSize)
      def sliderTrackHeight = c.getDouble(Theme.sliderTrackHeight)
      def sliderFillTrackActive = c.getObj[Paint](Theme.sliderFillTrackActive)
      def sliderFillTrackInactive = c.getObj[Paint](Theme.sliderFillTrackInactive)
      def sliderFillThumb = c.getObj[Paint](Theme.sliderFillThumb)
      def sliderStrokeThumb = c.getObj[Paint](Theme.sliderStrokeThumb)
      def sliderFillThumbActive = c.getObj[Paint](Theme.sliderFillThumbActive)
      def sliderStrokeThumbActive = c.getObj[Paint](Theme.sliderStrokeThumbActive)

      def textFieldCursorBlinkInterval = c.getInt(Theme.textFieldCursorBlinkInterval)
      def textFieldFillText = c.getObj[Paint](Theme.textFieldFillText)
      def textFieldFillPlaceholder = c.getObj[Paint](Theme.textFieldFillPlaceholder)
      def textFieldFillCursor = c.getObj[Paint](Theme.textFieldFillCursor)
      def textFieldFillSelectionActive = c.getObj[Paint](Theme.textFieldFillSelectionActive)
      def textFieldFillSelectionInactive = c.getObj[Paint](Theme.textFieldFillSelectionInactive)
      def textFieldFillBgActive = c.getObj[Paint](Theme.textFieldFillBgActive)
      def textFieldFillBgInactive = c.getObj[Paint](Theme.textFieldFillBgInactive)
      def textFieldBorderRadius = c.getInt(Theme.textFieldBorderRadius)
      def textFieldBorderActive = c.getObj[Paint](Theme.textFieldBorderActive)
      def textFieldBorderInactive = c.getObj[Paint](Theme.textFieldBorderInactive)
      def textFieldCursorWidth = c.getDouble(Theme.textFieldCursorWidth)
      def textFieldPaddingTop = c.getDouble(Theme.textFieldPaddingTop)
      def textFieldPaddingBottom = c.getDouble(Theme.textFieldPaddingBottom)
      def textFieldPaddingLeft = c.getDouble(Theme.textFieldPaddingLeft)
      def textFieldPaddingRight = c.getDouble(Theme.textFieldPaddingRight)



    }
  }
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) {
    import Value.*
    def apply[C](child: C, fontSize: Int, opts: Map[String, Value] = Map())(using F: Sync[F], C: Component[F, C]) = 
      Contextual[F] { context =>
        val scale = context.scale
        val fillText = opts.get(Theme.fillText).flatMap {
          case VObject(v: Paint) => Some(v)
          case _ => None
        }.getOrElse(paint.fill(0xFF000000))
        val fillGray = opts.get(Theme.fillGray).flatMap {
          case VObject(v: Paint) => Some(v)
          case _ => None
        }.getOrElse(paint.fill(0xFF808080))
        val ctx = for {
          faceUi <- F.delay { opts.get(Theme.faceUi).flatMap {
            case VObject(v: Typeface) => Some(v)
            case _ => None
          }.getOrElse(Typeface.makeDefault()) }
          font <- F.delay { Font(faceUi, (context.scale * fontSize).toFloat) }
          capHeight <- F.delay { font.getMetrics().getCapHeight() }
          leading = opts.get(Theme.leading).flatMap {
            case VDouble(v) => Some(v)
            case _ => None
          }.getOrElse((Math.round(capHeight) / context.scale).toDouble)
        } yield context.merge(
          Map(
              Theme.faceUi -> VObject(faceUi),
              Theme.fontUi -> VObject(font),
              Theme.leading -> VDouble(leading),
              Theme.fillText -> VObject(fillText),
              Theme.fillGray -> VObject(fillGray),

              Theme.buttonBgActive -> VObject(paint.fill(0xFFA2C7EE)),
              Theme.buttonBgHover  -> VObject(paint.fill(0xFFCFE8FC)),
              Theme.buttonBg       -> VObject(paint.fill(0xFFB2D7FE)),
              Theme.buttonBorderRadius -> VInt(4),
              Theme.buttonPaddingLeft -> VDouble(2 * leading),
              Theme.buttonPaddingTop -> VDouble(leading),
              Theme.buttonPaddingRight -> VDouble(2 * leading),
              Theme.buttonPaddingBottom -> VDouble(leading),

              Theme.toggleFillEnabled -> VObject(paint.fill(0xFF0080FF)),
              Theme.toggleFillDisabled -> VObject(paint.fill(0xFFD9D9D9)),
              Theme.toggleFillHandle   -> VObject(paint.fill(0xFFFFFFFF)),
              Theme.toggleFillEnabledActive -> VObject(paint.fill(0xFF0060E0)),
              Theme.toggleFillDisabledActive -> VObject(paint.fill(0xFFBBBBBB)),
              Theme.toggleFillHandleActive -> VObject(paint.fill(0xFFE0E0E0)),
              
              Theme.sliderThumbSize -> VDouble(16 * scale),
              Theme.sliderTrackHeight -> VDouble(2 * scale),
              Theme.sliderFillTrackActive -> VObject(paint.fill(0xFF0080FF)),
              Theme.sliderFillTrackInactive -> VObject(paint.fill(0xFFD9D9D9)),
              Theme.sliderFillThumb -> VObject(paint.fill(0xFFFFFFFF)),
              Theme.sliderStrokeThumb -> VObject(paint.stroke(0xFF0080FF, 2 * scale)),
              Theme.sliderFillThumbActive -> VObject(paint.fill(0xFFE0E0E0)),
              Theme.sliderStrokeThumbActive -> VObject(paint.stroke(0xFF0060E0, 2 * scale)),

              Theme.textFieldCursorBlinkInterval -> VInt(500),
              Theme.textFieldFillText -> VObject(fillText),
              Theme.textFieldFillPlaceholder -> VObject(fillGray),
              Theme.textFieldFillCursor -> VObject(fillText),
              Theme.textFieldFillSelectionActive -> VObject(paint.fill(0xFFB1D7FF)),
              Theme.textFieldFillSelectionInactive -> VObject(paint.fill(0xFFDDDDDD)),
              Theme.textFieldFillBgActive -> VObject(paint.fill(0xFFFFFFFF)),
              Theme.textFieldFillBgInactive -> VObject(paint.fill(0xFFF8F8F8)),
              Theme.textFieldBorderRadius -> VInt(4),
              Theme.textFieldBorderActive -> VObject(paint.stroke(0xFF749EE4, 1 * scale)),
              Theme.textFieldBorderInactive -> VObject(paint.stroke(0xFFCCCCCC, 1 * scale)),
              Theme.textFieldCursorWidth -> VDouble(1),
              Theme.textFieldPaddingTop -> VDouble(Math.round(capHeight) / scale),
              Theme.textFieldPaddingBottom -> VDouble(Math.round(capHeight) / scale),
              Theme.textFieldPaddingLeft -> VDouble(Math.round(capHeight / 2) / scale),
              Theme.textFieldPaddingRight -> VDouble(Math.round(capHeight / 2) / scale)


            )
          )
        ctx.map { theme =>
          WithContext(theme.merge(opts), child)
          
        }.toResource

      }
  }
  def defaultTheme[F[_]]: BuildOps[F] = BuildOps[F]()
}
export Theme.exts.*

