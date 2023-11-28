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

val bgColorActive = 0xFFA2C7EE
val bgColorHovered = 0xFFCFE8FC
val bgColor = 0xFFB2D7FE
def Button[F[_], C](onClick: F[Unit], child: C)(using F: Async[F], C: Component[F, C]) = 
  val withContext = WithContext[F, C](it => F.pure(it.copy(hovered = false, active = false)), child)
  for {
    centered <- center(withContext)
    padded = Padding(10, 5, centered)
    dyn <- Contextual[F] { context =>
      for {
        color <- 
          (if (context.active)
            F.delay { Paint() }.flatTap(it => F.delay { it.setColor(bgColorActive) })
          else if (context.hovered)
            F.delay { Paint() }.flatTap(it => F.delay { it.setColor(bgColorHovered) })
          else 
            F.delay { Paint() }.flatTap(it => F.delay { it.setColor(bgColor) })).toResource

        rect <- Rect[F](color, padded)
      } yield rect
    }
    clipRRect <- RRectClip[F](4f, dyn)
    clickable <- Clickable[F](clipRRect, _ => onClick)

  } yield clickable 
