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

// Contextual will ALWAYS rebuild on draw and measure
class Contextual[F[_], T](val childCtor: Context => Resource[F, T], val curChild: Ref[F, Option[T]],
  val childRect: Ref[F, IRect], val destructor: Ref[F, F[Unit]])(using F: Async[F], C:Component[F, T]) {
    private[modestui] def rebuild(ctx: Context): F[T] =
      for {
        child <- childCtor(ctx).allocated
        _ <- curChild.set(Some(child._1))
        _ <- destructor.get.flatten
        _ <- destructor.set(child._2)
      } yield child._1
  }

object Contextual {
  // TRUE! 
  sealed class BuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[T](childCtor: Context => Resource[F, T])(using F: Async[F], C: Component[F, T]): Resource[F, Contextual[F, T]] =
      Resource.make((Ref[F].of[Option[T]](None), Ref[F].of(IRect(0,0,0,0)), Ref[F].of(F.pure(()))).tupled.flatMap {
        case (curChild, childRect, destructor) =>
          F.delay { (curChild, childRect, destructor, new Contextual(childCtor, curChild, childRect, destructor)) }
      }) { case (_, _, destructor, _) =>
        destructor.get.flatten
      }.map(_._4)
  }
  def apply[F[_]] = new BuildOps[F]
}

given contextual_Component[F[_], T](using F: Async[F], C: Component[F, T]): Component[F, Contextual[F, T]] with 
  extension (self: Contextual[F, T]) {
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      for {
        child <- self.rebuild(ctx)
        _ <- self.childRect.set(rect)
        _ <- child.draw(ctx, rect, canvas)
      } yield ()
    def measure(ctx: Context, size: IPoint): F[IPoint] =
      self.rebuild(ctx).flatMap { child =>
        child.measure(ctx, size)
      }
    def event(ctx: Context, event: Event): F[Boolean] =
      self.curChild.get.flatMap { child => 
        child.map(_.event(ctx, event)).getOrElse(F.pure(false))
      }
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this)) *> self.curChild.get.flatMap { child =>
        child.traverse(_.map(ctx, cb)).void
      }
  }
// TODO: Will I get a redraw
class Dynamic[F[_], S, T](val childCtor: S => F[T], val ctx: SignallingRef[F, S], val curChild: Ref[F, T],
  val needsRedraw: Ref[F, Boolean])
  (using F: Async[F], C: Component[F, T]) {
  
  private[modestui] def setup: Resource[F, Unit] =
    ctx.discrete.evalTap(it => childCtor(it).flatMap(c => curChild.set(c) *> needsRedraw.set(true))).compile.drain.background.void
}

object Dynamic {
  // TRUE!
  sealed class DynamicBuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[S, T](ctx: SignallingRef[F, S])(childCtor: S => F[T])(using F: Async[F], C: Component[F, T]): Resource[F, Dynamic[F, S, T]] = {
      for {
        curStateVal <- ctx.get.toResource
        curChildVal <- childCtor(curStateVal).toResource
        curChild <- Ref[F].of(curChildVal).toResource
        needsRedraw <- Ref[F].of(false).toResource
        dyn <- F.delay { new Dynamic[F, S, T](childCtor, ctx, curChild, needsRedraw) }.toResource.flatTap(_.setup)
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
        needsRedraw <- self.needsRedraw.get 
        _ <- self.needsRedraw.set(false)
        res <- child.event(ctx, event)
      } yield res || needsRedraw
  }
