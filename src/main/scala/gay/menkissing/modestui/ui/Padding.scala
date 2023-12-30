package gay.menkissing.modestui.ui

import gay.menkissing.modestui.*

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas

class Padding[T](val left: Float, val top: Float, val right: Float, val bottom: Float, val maChild: T)

object Padding {
  def apply[T](p: Float, child: T) = new Padding[T](p, p, p, p, child)
  def apply[T](w: Float, h: Float, child: T) = new Padding[T](w, h, w, h, child)
}

given padding_Component[F[_], T](using F: Async[F], C: Component[F, T]): AWrapper[F, Padding[T], T] with
  extension (self: Padding[T]) {
    def child = F.pure(self.maChild)
    override def measure(ctx: Context, size: IPoint): F[IPoint] =
      val left = ctx.scale * self.left
      val top = ctx.scale * self.top
      val right = ctx.scale * self.right
      val bottom = ctx.scale * self.bottom 
      val childSizeInner = IPoint((size.getX - left - right).toInt, (size.getY - top - bottom).toInt)
      for {
        childSize <- self.maChild.measure(ctx, childSizeInner)
      } yield IPoint(
        (childSize.getX + left + right).toInt,
        (childSize.getY + top + bottom).toInt
        )
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      val left = ctx.scale * self.left
      val top = ctx.scale * self.top
      val right = ctx.scale * self.right
      val bottom = ctx.scale * self.bottom
      val width = rect.getWidth - left - right
      val height = rect.getHeight - top - bottom
      val childRect = IRect.makeXYWH(rect.getLeft + left.toInt, rect.getTop + top.toInt, 
        math.max(0, width).toInt, math.max(0, height).toInt)
      self.maChild.draw(ctx, rect, canvas)

  }
