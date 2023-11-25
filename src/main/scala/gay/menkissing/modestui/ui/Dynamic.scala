package gay.menkissing.modestui.ui

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.syntax.all.*
import fs2.concurrent.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.Component
import io.github.humbleui.jwm.Event

class Dynamic[F[_], S, T](val childCtor: S => F[T], val ctx: SignallingRef[F, S], val curChild: Ref[F, T], topic: Topic[F, Event])
  (using F: Async[F], C: Component[F, T]) extends HasTopic[F](topic) {
  
  private[modestui] def setup: Resource[F, Unit] =
    ctx.discrete.evalTap(it => childCtor(it).flatMap(c => curChild.set(c))).compile.drain.background.void
}

object Dynamic {
  // TRUE!
  sealed class DynamicBuildOps[F[_]](val underlying: Boolean = true) extends AnyVal {
    def apply[S, T](ctx: SignallingRef[F, S])(childCtor: S => F[T])(using F: Async[F], C: Component[F, T]): Resource[F, Dynamic[F, S, T]] = {
      for {
        topic <- Topic[F, Event].toResource
        curStateVal <- ctx.get.toResource
        curChildVal <- childCtor(curStateVal).toResource
        curChild <- Ref[F].of(curChildVal).toResource
        dyn <- F.delay { new Dynamic[F, S, T](childCtor, ctx, curChild, topic) }.toResource.flatTap(_.setup)
      } yield dyn
    }
  }
  def apply[F[_]]: DynamicBuildOps[F] = new DynamicBuildOps[F]
}
