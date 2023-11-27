package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import gay.menkissing.modestui.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.instance.*

import io.github.humbleui.types.{IPoint, IRect}
import io.github.humbleui.skija.Canvas
import io.github.humbleui.jwm.Event

class Clickable[F[_], C](onClick: Event => F[Unit], onClickCapture: Event => F[Unit], child: C,
  childRect: Ref[F, IRect], hovered: Ref[F, Boolean], pressed: Ref[F, Boolean], clicks: Ref[F, Int],
  lastClick: Ref[F, Int])
