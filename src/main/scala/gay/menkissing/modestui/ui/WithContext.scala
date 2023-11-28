package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.jwm.Event
import io.github.humbleui.skija.Canvas

import gay.menkissing.modestui.*
import gay.menkissing.modestui.core.*
import gay.menkissing.modestui.instance.*

class WithContext[F[_], T](val modifier: Context => F[Context], val child: T)

// TODO: do I need a good builder that does resource?

given withContext_Component[F[_], T](using F: Async[F], C: Component[F, T]): Component[F, WithContext[F, T]] with 
  extension (self: WithContext[F, T]) {
    def measure(ctx: Context, size: IPoint): F[IPoint] =
      self.modifier(ctx).flatMap { ctx => 
        self.child.measure(ctx, size)
      }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      self.modifier(ctx).flatMap { ctx =>
        self.child.draw(ctx, rect, canvas)
      }
    def event(ctx: Context, event: Event): F[Boolean] =
      self.modifier(ctx).flatMap { ctx =>
        self.child.event(ctx, event)
      }
    def map(ctx: Context, cb: Instance[Component[F, _]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this)) *>
      self.modifier(ctx).flatMap { ctx =>
        self.child.map(ctx, cb)
      }
  }
