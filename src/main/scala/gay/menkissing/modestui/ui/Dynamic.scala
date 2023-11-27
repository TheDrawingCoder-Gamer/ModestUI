package gay.menkissing.modestui.ui

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.syntax.all.*
import fs2.concurrent.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.*
import gay.menkissing.modestui.instance.*
import io.github.humbleui.jwm.Event
import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas

// TODO: Will I get a redraw
class Dynamic[F[_], S, T](val childCtor: S => F[T], val ctx: SignallingRef[F, S], val curChild: Ref[F, T])
  (using F: Async[F], C: Component[F, T]) {
  
  private[modestui] def setup: Resource[F, Unit] =
    ctx.discrete.evalTap(it => childCtor(it).flatMap(c => curChild.set(c))).compile.drain.background.void
}

object Dynamic {
  // TRUE!
  sealed class DynamicBuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[S, T](ctx: SignallingRef[F, S])(childCtor: S => F[T])(using F: Async[F], C: Component[F, T]): Resource[F, Dynamic[F, S, T]] = {
      for {
        curStateVal <- ctx.get.toResource
        curChildVal <- childCtor(curStateVal).toResource
        curChild <- Ref[F].of(curChildVal).toResource
        dyn <- F.delay { new Dynamic[F, S, T](childCtor, ctx, curChild) }.toResource.flatTap(_.setup)
      } yield dyn
    }
  }
  def apply[F[_]]: DynamicBuildOps[F] = new DynamicBuildOps[F]
}

given dynamic_Component[F[_], S, T](using F: Async[F], C: Component[F, T]): Component[F, Dynamic[F, S, T]] with
  extension (self: Dynamic[F, S, T]) {
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = 
      for {
        child <- self.curChild.get
        _ <- child.draw(ctx, rect, canvas)
      } yield ()
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] =
      for {
        child <- self.curChild.get
        _ <- cb(Instance(self)(using this))
        _ <- child.map(ctx, cb)
      } yield ()
    def measure(ctx: Context, rect: IPoint): F[IPoint] =
      self.curChild.get.flatMap(_.measure(ctx, rect))
    def event(ctx: Context, event: Event): F[Boolean] =
      for {
        child <- self.curChild.get
        res <- child.event(ctx, event)
      } yield res
  }
