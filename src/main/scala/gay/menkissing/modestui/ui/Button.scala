package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import io.github.humbleui.jwm.Event
import io.github.humbleui.types.{IPoint, IRect}
import io.github.humbleui.skija.Paint

import gay.menkissing.modestui.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.ui.Theme.exts.*

def Button[F[_], C](onClick: F[Unit], child: C)(using F: Async[F], C: Component[F, C]) =
  import Value.*
  val withContext = WithContext[F, C](Context(
    Map(Context.hovered -> VBoolean(false), Context.active -> VBoolean(false))
  ), child)
  for {
    centered <- center(withContext)
    padded = Padding(10, 5, centered)
    dyn <- Contextual[F] { context =>
      val color = 
        if (context.getBool(Context.active).get)
          context.buttonBgActive.get
        else if (context.getBool(Context.hovered).get)
          context.buttonBgHover.get
        else
          context.buttonBg.get
      Rect[F](color, padded)
    }
    clipRRect <- RRectClip[F](4f, dyn)
    clickable <- Clickable[F](clipRRect, _ => onClick)

  } yield clickable 
