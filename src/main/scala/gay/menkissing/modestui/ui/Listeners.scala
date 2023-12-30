package gay.menkissing.modestui.ui

import gay.menkissing.modestui.*
import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.implicits.*

import io.github.humbleui.jwm.Event
import io.github.humbleui.skija.Canvas
import io.github.humbleui.types.IRect


/**
 * An event listener.
 * 
 * Callback returns a result.
 * If redraw is false, captured doesn't matter, as the child will always receive the event. 
 * Capture means that if redraw is true, child won't be called
 */
class EventListener[F[_], T](val callback: PartialFunction[(Context, Event), F[Boolean]], val child: T, val capture: Boolean)

object EventListener {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[T](child: T, capture: Boolean = false)(callback: PartialFunction[(Context, Event), F[Boolean]]) =
      new EventListener[F, T](callback, child, capture)
  }
  def apply[F[_]] = new BuildOps[F]
}

given component_EventListener[F[_], T](using F: Async[F], C: Component[F, T]): AWrapper[F, EventListener[F, T], T] with {
  extension (self: EventListener[F, T]) {
    def child = self.child.pure[F]
    def draw(ctx: Context, rect: IRect, canvas: Canvas) =
      self.child.draw(ctx, rect, canvas)
    override def event(ctx: Context, event: Event) =
      lazyOrF(
        if (self.capture) self.callback.applyOrElse((ctx, event), _ => F.pure(false)) else F.pure(false),
        self.child.event(ctx, event),
        if (!self.capture) self.callback.applyOrElse((ctx, event), _ => F.pure(false)) else F.pure(false)
        )

  }
}

