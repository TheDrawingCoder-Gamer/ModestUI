package gay.menkissing.modestui;

import io.github.humbleui.types.{IPoint, IRect};
import io.github.humbleui.jwm.Event;
import io.github.humbleui.skija.Canvas
import gay.menkissing.modestui.instance.*

trait Component[F[_], I] {
  extension (self: I) {
    def measure(p: IPoint): F[IPoint]
    def draw(rect: IRect, canvas: Canvas): F[Unit]
    def event(event: Event): F[Unit]
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit]
  }
}
