package gay.menkissing.modestui

import io.github.humbleui.jwm.Window
import cats.effect.*
import io.github.humbleui.types.IPoint
// Context of shit???
// I fucking hate this : (
// This is redone every draw call (?)
case class Context(
    window: Window,
    // UI & text scaling on this windows screen
    scale: Float,
    mousePos: IPoint,
    active: Boolean,
    hovered: Boolean
  )


