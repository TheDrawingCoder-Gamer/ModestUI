package gay.menkissing.modestui.ui;

import gay.menkissing.modestui.*
import cats.effect.*
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas
import fs2.concurrent.Topic
class HAlign[F[_], T](val childCoeff: Float, val coeff: Float, val myChild: T)

object HAlign {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[T](coeff: Float, child: T)(using F: Async[F], C: Component[F, T]) =
      new HAlign[F, T](coeff, coeff, child)
  }
  def apply[F[_]] = new BuildOps[F]
}
given halign_Component[F[_], T](using C: Component[F, T], S: Sync[F], M: Monad[F]): AWrapper[F, HAlign[F, T], T] with {
  extension (self: HAlign[F, T]) {
    def child = M.pure { self.myChild }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      for {
        childSize <- self.myChild.measure(ctx, IPoint(rect.getWidth, rect.getHeight))
        left = (rect._left + (rect.getWidth * self.coeff) - (childSize._x * self.childCoeff))
        childRect = IRect.makeXYWH(left.toInt, rect._top, childSize._x, rect.getHeight)
        _ <- self.myChild.draw(ctx, childRect, canvas)
      } yield ()
    }
  }
}


class VAlign[F[_], T](val childCoeff: Float, val coeff: Float, val myChild: T)
object VAlign {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[T](coeff: Float, child: T)(using F: Async[F], C: Component[F, T]) =
      new VAlign[F, T](coeff, coeff, child)
  }
  def apply[F[_]] = new BuildOps[F]
}
given valign_Component[F[_], T](using C: Component[F, T], S: Sync[F], M: Monad[F]): AWrapper[F, VAlign[F, T], T] with {
  extension (self: VAlign[F, T]) {
    def child = M.pure { self.myChild }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      for {
        childSize <- self.myChild.measure(ctx, IPoint(rect.getWidth, rect.getHeight))
        top = rect._top + (rect.getHeight * self.coeff) - (childSize._y * self.childCoeff)
        childRect = IRect.makeXYWH(rect._left, top.toInt, rect.getWidth, childSize._y)
        _ <- self.myChild.draw(ctx, childRect, canvas)
      } yield ()
    }
  }
}



// TRUE!
sealed case class CenterApplied[F[_]](val underlying: Boolean = true) extends AnyVal {
  def apply[T](child: T)(using Async[F], Component[F, T]) =
    VAlign[F](0.5, HAlign[F](0.5, child))
}
def center[F[_]] = new CenterApplied[F]
