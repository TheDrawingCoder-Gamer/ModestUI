package gay.menkissing.modestui.ui;

import gay.menkissing.modestui.{Component, Context}
import gay.menkissing.modestui.instance.*
import gay.menkissing.modestui.core.*
import cats.effect.*
import cats.effect.syntax.all.*
import cats.*
import cats.implicits.*
import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas
import fs2.concurrent.Topic
import io.github.humbleui.jwm.Event
class HAlign[F[_], T](val childCoeff: Float, val coeff: Float, val myChild: T, topic: Topic[F, Event]) extends HasTopic[F](topic)

object HAlign {
  def apply[F[_], T](coeff: Float, child: T)(using F: Async[F]): Resource[F, HAlign[F, T]] =
    for {
      topic <- Topic[F, Event].toResource
      halign <- F.delay { new HAlign(coeff, coeff, child, topic)}.toResource
    } yield halign
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


class VAlign[F[_], T](val childCoeff: Float, val coeff: Float, val myChild: T, topic: Topic[F, Event]) extends HasTopic[F](topic)
object VAlign {
  def apply[F[_], T](coeff: Float, child: T)(using F: Async[F]): Resource[F, VAlign[F, T]] =
    for {
      topic <- Topic[F, Event].toResource
      valign <- F.delay { new VAlign(coeff, coeff, child, topic) }.toResource
    } yield valign
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




def center[F[_], T](child: T)(using Async[F]) =
  // forbidden
  VAlign[F, T](0.5, child).flatMap(HAlign(0.5, _))
