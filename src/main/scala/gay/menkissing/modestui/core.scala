package gay.menkissing.modestui.core;

import gay.menkissing.modestui.Component
import cats.*
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.instance.{*, given}
import cats.implicits.*

trait ATerminal[F[_], I](using app: Applicative[F]) extends Component[F, I] {
  extension (self: I) {
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this))
    def event(event: Event): F[Unit] = app.pure(())
  }
}

trait AWrapper[F[_], I](using M: Monad[F]) extends Component[F, I] {

  extension (self: I) {
    def child: F[Instance[[X] =>> Component[F, X]]]
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.child >>= { child =>
      
      cb(Instance(self)(using this)) *> child.instance.map(child.item)(cb)
    }
    def event(event: Event): F[Unit] = self.child >>= { child =>
      child.instance.event(child.item)(event)
    }
  }
}

trait AContainer[F[_], I](using M: Monad[F]) extends Component[F, I] {
  extension (self: I) {
    def children: F[List[Instance[[X] =>> Component[F, X]]]]
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.children >>= { children => 
      cb(Instance(self)(using this)) *> children.traverse(child => child.instance.map(child.item)(cb)).void
    }
    def event(event: Event): F[Unit] = self.children >>= { children => 
      children.traverse(child => child.instance.event(child.item)(event)).void
    }
  }
}
