package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import io.github.humbleui.skija.{Paint, Canvas}
import io.github.humbleui.types.{IRect, RRect, IPoint}
import io.github.humbleui.jwm.Event

import gay.menkissing.modestui.*

class Rect[F[_], T](val paint: Paint, val child: T, val childRect: Ref[F, IRect])

object Rect {
  // TRUE!
  sealed case class BuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[T](paint: Paint, child: T)(using F: Sync[F], C: Component[F, T]): Resource[F, Rect[F, T]] =
      Ref[F].of(IRect(0, 0, 0, 0)).flatMap { childRect =>
        F.delay { new Rect(paint, child, childRect) }
      }.toResource
    def apply[T](child: T)(using F: Sync[F], C: Component[F, T]): Resource[F, Rect[F, T]] =
      apply(new Paint(), child)
  }
  def apply[F[_]] = new BuildOps[F]
}

given rect_Component[F[_], T](using F: Sync[F], C: Component[F, T]): Component[F, Rect[F, T]] with
  extension (self: Rect[F, T]) {
    def measure(ctx: Context, size: IPoint): F[IPoint] =
      self.child.measure(ctx, size)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      for {
        _ <- self.childRect.set(rect)
        _ <- F.delay { canvas.drawRect(rect.toRect, self.paint) }
        _ <- self.child.draw(ctx, rect, canvas)
      } yield ()
    def event(ctx: Context, event: events.MEvent): F[Boolean] =
      self.child.event(ctx, event)
    def map(ctx: Context, cb: Instance[Component[F, _]] => F[Unit]): F[Unit] = 
      cb(Instance(self)(using this)) *> self.child.map(ctx, cb)
  }

class RoundedRect[F[_], T](val radius: Float, val paint: Paint, val child: T, val childRect: Ref[F, IRect])

object RoundedRect {
  // TRUE!
  sealed case class BuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[T](radius: Float, paint: Paint, child: T)(using F: Sync[F], C: Component[F, T]): Resource[F, RoundedRect[F, T]] =
      Ref[F].of(IRect(0, 0, 0, 0)).flatMap { childRect =>
        F.delay { new RoundedRect(radius, paint, child, childRect) }
      }.toResource
    def apply[T](radius: Float, child: T)(using F: Sync[F], C: Component[F, T]): Resource[F, RoundedRect[F, T]] =
      apply(radius, new Paint(), child)
  }
  def apply[F[_]] = new BuildOps[F]
}

given roundedrect_Component[F[_], T](using F: Sync[F], C: Component[F, T]): Component[F, RoundedRect[F, T]] with
  extension (self: RoundedRect[F, T]) {
    def measure(ctx: Context, size: IPoint): F[IPoint] =
      self.child.measure(ctx, size)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      for {
        _ <- self.childRect.set(rect)
        rrect = RRect.makeXYWH(rect.getLeft, rect.getTop, rect.getWidth, rect.getHeight, (ctx.scale * self.radius).toFloat)
        _ <- F.delay { canvas.drawRRect(rrect, self.paint) }
        _ <- self.child.draw(ctx, rect, canvas)
      } yield ()
    def event(ctx: Context, event: events.MEvent): F[Boolean] =
      self.child.event(ctx, event)
    def map(ctx: Context, cb: Instance[Component[F, _]] => F[Unit]): F[Unit] = 
      cb(Instance(self)(using this)) *> self.child.map(ctx, cb)
  }
