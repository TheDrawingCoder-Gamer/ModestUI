package gay.menkissing.modestui;

import io.github.humbleui.types.{IPoint, IRect};
import io.github.humbleui.skija.Canvas
import gay.menkissing.modestui.events.*

trait Component[F[_], I] {
  extension (self: I) {
    def event(ctx: Context, event: events.MEvent): F[Boolean]
    def measure(ctx: Context, p: IPoint): F[IPoint]
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit]
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit]
  }
}

