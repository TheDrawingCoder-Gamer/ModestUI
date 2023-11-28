package gay.menkissing.modestui.core;

import gay.menkissing.modestui.{Component, Context}
import cats.*
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.instance.{*, given}
import cats.implicits.*
import io.github.humbleui.types.{IPoint, IRect}
import fs2.concurrent.Topic

trait HasTopic[F[_]](val thisTopic: Topic[F, Event])

trait ATerminal[F[_], I](using app: Applicative[F]) extends Component[F, I] {
  extension (self: I) {
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this))
    def event(ctx: Context, event: Event): F[Boolean] = app.pure(false)
  }
}

trait AWrapper[F[_], I, C](using M: Monad[F], C: Component[F, C]) extends Component[F, I] {

  extension (self: I) {
    def child: F[C]
    def measure(ctx: Context, size: IPoint): F[IPoint] = 
      self.child >>= { child => child.measure(ctx, size) }
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.child >>= { child =>
      cb(Instance(self)(using this)) *> child.map(ctx, cb)
    }
    def event(ctx: Context, event: Event): F[Boolean] =
      child.flatMap(_.event(ctx, event))
  }
}

trait AContainer[F[_], I](using M: Monad[F]) extends Component[F, I] {
  extension (self: I) {
    def children: F[List[Instance[[X] =>> Component[F, X]]]]
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.children >>= { children => 
      cb(Instance(self)(using this)) *> children.traverse(child => child.instance.map(child.item)(ctx, cb)).void
    }
    def event(ctx: Context, event: Event): F[Boolean] =
      self.children.flatMap { children =>
        children.traverse(child => child.instance.event(child.item)(ctx, event)).map(_.exists(identity))
      }
  }
}

def rectContains(rect: IRect, pos: IPoint): Boolean =
  rect.getLeft <= pos.getX
  && pos.getX < rect.getRight
  && rect.getTop <= pos.getY
  && pos.getY < rect.getBottom

val doubleClickThresholdMS = 500

def eagerOrF[F[_]](thingies: F[Boolean]*)(using F: Applicative[F]): F[Boolean] =
  thingies.toList.sequence.map(_.exists(identity))

def lazyOrF[F[_]](thingies: F[Boolean]*)(using F: Monad[F]): F[Boolean] =
  forbiddenLazyOrF[F](thingies.toList)
private def forbiddenLazyOrF[F[_]](thingies: List[F[Boolean]])(using F: Monad[F]): F[Boolean] =
  thingies match {
    case next :: rest =>
      next.flatMap { good =>
        if (!good)
          forbiddenLazyOrF(rest)
        else
          F.pure(true) // !!
      }
    case Nil =>
      F.pure(false)
  }

