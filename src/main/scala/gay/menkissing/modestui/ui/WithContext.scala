package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas

import gay.menkissing.modestui.*

class WithContext[F[_], T](val ctx: Context, val child: T) {
  def context(other: Context) =
    other.merge(ctx)
}

// TODO: do I need a good builder that does resource?

given withContext_Component[F[_], T](using F: Sync[F], C: Component[F, T]): Component[F, WithContext[F, T]] with 
  extension (self: WithContext[F, T]) {
    def measure(ctx: Context, size: IPoint): F[IPoint] =
      self.child.measure(self.context(ctx), size)
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      self.child.draw(self.context(ctx), rect, canvas)
    def event(ctx: Context, event: events.MEvent): F[Boolean] =
      self.child.event(self.context(ctx), event)
    def map(ctx: Context, cb: Instance[Component[F, _]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this)) *>
      self.child.map(self.context(ctx), cb)
  }
