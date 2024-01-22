package gay.menkissing.modestui.events

import io.github.humbleui.jwm.*
import io.github.humbleui.jwm.skija.EventFrameSkija
import io.github.humbleui.types.IPoint
import io.github.humbleui.skija.Surface

sealed trait MEvent

// marker trait for events that must be executed syncronously
trait TimedEvent {}

object MEvent {
  def fromJWM(e: Event): MEvent = {
    e match {
      case ee: EventMouseButton =>
        MEventMouseButton(ee.getX, ee.getY, ee.getButton, ee.isPressed, ee._modifiers)
      case ee: EventMouseMove =>
        MEventMouseMove(ee.getX, ee.getY, ee.getMovementX, ee.getMovementY, ee._buttons, ee._modifiers)
      case ee: EventMouseScroll =>
        MEventMouseScroll(ee.getDeltaX, ee.getDeltaY, ee.getDeltaChars, ee.getDeltaLines, ee.getDeltaPages, ee.getX, ee.getY, ee._modifiers)
      case ee: EventKey =>
        MEventKey(ee.getKey, ee.isPressed, ee._modifiers, ee.getLocation)
      case ee: EventTextInput =>
        MEventTextInput(ee.getText, ee.getReplacementStart, ee.getReplacementEnd)
      case ee: EventTextInputMarked =>
        MEventTextInputMarked(ee.getText, ee.getSelectionStart, ee.getSelectionEnd, ee.getReplacementStart, ee.getReplacementEnd)
      case ee: EventTouchStart =>
        MEventTouchStart(ee.getId, ee.getFracX, ee.getFracY, ee.getDeviceId, ee.getDeviceWidth, ee.getDeviceHeight, ee.getTouchType)
      case ee: EventTouchMove =>
        MEventTouchMove(ee.getId, ee.getFracX, ee.getFracY)
      case ee: EventTouchEnd =>
        MEventTouchEnd(ee.getId)
      case ee: EventTouchCancel =>
        MEventTouchCancel(ee.getId)
      case _: EventWindowClose =>
        MEventWindowClose
      case _: EventWindowCloseRequest =>
        MEventWindowCloseRequest
      case _: EventWindowFocusIn =>
        MEventWindowFocusIn
      case _: EventWindowFocusOut =>
        MEventWindowFocusOut
      case _: EventWindowFullScreenEnter =>
        MEventWindowFullscreenEnter
      case _: EventWindowFullScreenExit =>
        MEventWindowFullscreenExit
      case _: EventWindowMaximize =>
        MEventWindowMaximize
      case _: EventWindowMinimize =>
        MEventWindowMinimize
      case ee: EventWindowMove =>
        MEventWindowMove(ee.getWindowLeft, ee.getWindowTop)
      case ee: EventWindowResize =>
        MEventWindowResize(ee.getWindowWidth, ee.getWindowHeight, ee.getContentWidth, ee.getContentHeight)
      case _: EventWindowRestore =>
        MEventWindowRestore
      case _: EventWindowScreenChange =>
        MEventWindowScreenChange
      case ee: EventFrameSkija =>
        MEventFrameSkija(ee.getSurface)
      case ee: EventFrame =>
        MEventFrame
      case MEventSignalUpdated =>
        MEventSignalUpdated
      case _ => null
    } 
  }
}

case object MEventSignalUpdated extends Event, MEvent

sealed trait MModifierEvent extends MEvent {
  def modifiers: Int

  def isModifierDown(modifier: KeyModifier) =
    (modifiers & modifier._mask) != 0
}

sealed trait MMouseEvent extends MModifierEvent {
  def x: Int
  def y: Int
  def modifiers: Int


  def withPoint(p: IPoint): MMouseEvent 
}

case class MEventMouseButton(x: Int, y: Int, mouseButton: MouseButton, isPressed: Boolean, modifiers: Int) extends MMouseEvent {
  def withPoint(p: IPoint) = 
    this.copy(x = p.getX, y = p.getY)
}

case class MEventMouseMove(x: Int, y: Int, movementX: Int, movementY: Int,
  buttons: Int, modifiers: Int) extends MMouseEvent
  {
    def isButtonDown(button: MouseButton) =
      // hack
      (buttons & button._mask) != 0
    def withPoint(p: IPoint) =
      this.copy(x = p.getX, y = p.getY)
  }

case class MEventMouseScroll(deltaX: Float, deltaY: Float, deltaChars: Float, deltaLines: Float, deltaPages: Float, x: Int, y: Int, modifiers: Int) extends MMouseEvent {
  def withPoint(p: IPoint) =
    this.copy(x = p.getX, y = p.getY)
}

case class MEventKey(key: Key, isPressed: Boolean, modifiers: Int, keyLocation: KeyLocation) extends MModifierEvent

case class MEventTextInput(text: String, replacementStart: Int, replacementEnd: Int) extends MEvent

case class MEventTextInputMarked(text: String, selectionStart: Int, selectionEnd: Int, replacementStart: Int, replacementEnd: Int) extends MEvent

sealed trait MTouchEvent extends MEvent {
  def id: Int
}
case class MEventTouchStart(id: Int, fracX: Float, fracY: Float, deviceId: Int, deviceWidth: Float, deviceHeight: Float, touchType: TouchType) extends MTouchEvent

case class MEventTouchMove(id: Int, fracX: Float, fracY: Float) extends MTouchEvent

case class MEventTouchEnd(id: Int) extends MTouchEvent
case class MEventTouchCancel(id: Int) extends MTouchEvent

case object MEventWindowClose extends MEvent
case object MEventWindowCloseRequest extends MEvent
case object MEventWindowFocusIn extends MEvent
case object MEventWindowFocusOut extends MEvent
case object MEventWindowFullscreenEnter extends MEvent
case object MEventWindowFullscreenExit extends MEvent
case object MEventWindowMaximize extends MEvent
case object MEventWindowMinimize extends MEvent
case class MEventWindowMove(windowLeft: Int, windowTop: Int) extends MEvent
case class MEventWindowResize(windowWidth: Int, windowHeight: Int, contentWidth: Int, contentHeight: Int) extends MEvent
case object MEventWindowRestore extends MEvent
case object MEventWindowScreenChange extends MEvent

case class MEventFrameSkija(surface: Surface) extends MEvent, TimedEvent
case object MEventFrame extends MEvent, TimedEvent
