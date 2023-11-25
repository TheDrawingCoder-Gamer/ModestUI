package gay.menkissing.modestui.core;

import gay.menkissing.modestui.Component
import cats.*
import io.github.humbleui.jwm.Event
import gay.menkissing.modestui.instance.{*, given}
import cats.implicits.*
import io.github.humbleui.types.IPoint
import fs2.concurrent.Topic

trait HasTopic[F[_]](val thisTopic: Topic[F, Event])
trait ATerminal[F[_], I <: HasTopic[F]](using app: Applicative[F]) extends Component[F, I] {
  extension (self: I) {
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] =
      cb(Instance(self)(using this))
    def topic = self.thisTopic
    def subscribe(maxQueued: Int): fs2.Stream[F, Event] =
      topic.subscribe(maxQueued)
  }
}

trait AWrapper[F[_], I <: HasTopic[F], C](using M: Monad[F], C: Component[F, C]) extends Component[F, I] {

  extension (self: I) {
    def child: F[C]
    def measure(size: IPoint): F[IPoint] = 
      self.child >>= { child => child.measure(size) }
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.child >>= { child =>
      
      cb(Instance(self)(using this)) *> child.map(cb)
    }
    def topic = self.thisTopic
    def subscribe(maxQueued: Int): fs2.Stream[F, Event] =
      topic.subscribe(maxQueued).evalTap(it => child.flatMap(_.topic.publish1(it)))
    
  }
}

trait AContainer[F[_], I <: HasTopic[F]](using M: Monad[F]) extends Component[F, I] {
  extension (self: I) {
    def children: F[List[Instance[[X] =>> Component[F, X]]]]
    def map(cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.children >>= { children => 
      cb(Instance(self)(using this)) *> children.traverse(child => child.instance.map(child.item)(cb)).void
    }
    def topic = self.thisTopic
    def subscribe(maxQueued: Int): fs2.Stream[F, Event] = fs2.Stream.eval { self.children } >>= { children =>
      topic.subscribe(maxQueued).evalTap(it => children.traverse(child => child.instance.topic(child.item).publish1(it)))
    }
  }
}
