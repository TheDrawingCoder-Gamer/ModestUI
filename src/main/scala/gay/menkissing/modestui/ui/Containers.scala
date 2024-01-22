package gay.menkissing.modestui.ui

import cats.*
import cats.implicits.*
import cats.effect.*
import cats.effect.syntax.all.*
import fs2.concurrent.*
import gay.menkissing.modestui.{*, given}

import io.github.humbleui.jwm.Event
import io.github.humbleui.types.{IRect, IPoint}
import io.github.humbleui.skija.Canvas

object Container {
  enum Mode {
    case Hug, Stretch
  }
}
case class Child[F[_]](mode: Container.Mode, factor: Float, child: Instance[Component[F, _]])

object Child {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def hug[C](child: C)(using F: Sync[F], C: Component[F, C]) =
      Child[F](Container.Mode.Hug, 0, child)
    def stretch[C](factor: Float, child: C)(using F: Sync[F], C: Component[F, C]) =
      Child[F](Container.Mode.Stretch, factor, child)
  }
  def apply[F[_]] = new BuildOps[F]
}

class Column[F[_]](val modes: List[Container.Mode], val factors: List[Float], val children: List[Instance[Component[F, _]]])

object Column {
  // TRUE!
  def apply[F[_]](cs: Child[F]*): Column[F] = apply[F](cs.toList)

  def apply[F[_]](cs: List[Child[F]]): Column[F] = {
    val (modes, factors, children) = cs.unzip3(using it => (it.mode, it.factor, it.child))
    new Column[F](modes, factors, children)
  }

}

given column_Component[F[_]](using F: Sync[F]): AContainer[F, Column[F]] with {
  extension (self: Column[F]) {
    def children = self.children.pure[F]
    def measure(ctx: Context, size: IPoint): F[IPoint] = {
      self.children.foldM(IPoint(0, 0)) { (p, child) =>
        for { 
          childSize <- child.instance.measure(child.item)(ctx, size)
        } yield IPoint(math.max(p.getX, childSize.getX), math.max(p.getY, childSize.getY))
      }
    }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      val cs = IPoint(rect.getWidth, rect.getHeight)
      for {
        known <- self.modes.zip(self.children).traverse {
          case (Container.Mode.Hug, child) =>
            child.instance.measure(child.item)(ctx, cs).map(Some(_))
          case _ => None.pure[F]
        }
        space = rect.getHeight - known.map {
          case Some(v) => v.getY
          case _ => 0
        }.sum
        stretch = self.modes.zip(self.factors).map {
          case (Container.Mode.Stretch, factor) =>
            factor
          case _ => 0
        }.sum
        _ <- known.lazyZip(self.modes).lazyZip(self.factors).lazyZip(self.children).toList.foldM(0) { case (height, (size, mode, factor, child)) =>
          val childHeight =
            mode match
              case Container.Mode.Hug =>
                size.get.getY
              case Container.Mode.Stretch =>
                math.round((space / stretch) * factor)
          val childRect = IRect.makeXYWH(rect.getLeft, rect.getTop + height, math.max(0, rect.getWidth), math.max(0, childHeight))
          child.instance.draw(child.item)(ctx, childRect, canvas) *> F.pure(height + childHeight)

        }
      } yield ()
      
    }
  }
}

class Row[F[_]](val modes: List[Container.Mode], val factors: List[Float], val children: List[Instance[Component[F, _]]])

object Row {
  // TRUE!
  def apply[F[_]](cs: Child[F]*): Row[F] = apply[F](cs.toList)

  def apply[F[_]](cs: List[Child[F]]): Row[F] = {
    val (modes, factors, children) = cs.unzip3(using it => (it.mode, it.factor, it.child))
    new Row[F](modes, factors, children)
  }

}

given row_Component[F[_]](using F: Sync[F]): AContainer[F, Row[F]] with {
  extension (self: Row[F]) {
    def children = self.children.pure[F]
    def measure(ctx: Context, size: IPoint): F[IPoint] = {
      self.children.foldM(IPoint(0, 0)) { (p, child) =>
        for { 
          childSize <- child.instance.measure(child.item)(ctx, size)
        } yield IPoint(math.max(p.getX, childSize.getX), math.max(p.getY, childSize.getY))
      }
    }
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] = {
      val cs = IPoint(rect.getWidth, rect.getHeight)
      for {
        known <- self.modes.zip(self.children).traverse {
          case (Container.Mode.Hug, child) =>
            child.instance.measure(child.item)(ctx, cs).map(Some(_))
          case _ => None.pure[F]
        }
        space = rect.getWidth - known.map {
          case Some(v) => v.getX
          case _ => 0
        }.sum
        stretch = self.modes.zip(self.factors).map {
          case (Container.Mode.Stretch, factor) =>
            factor
          case _ => 0
        }.sum
        _ <- (known lazyZip self.modes lazyZip self.factors lazyZip self.children).toList.foldM(0) { case (width, (size, mode, factor, child)) =>
          val childWidth =
            mode match
              case Container.Mode.Hug =>
                size.get.getX
              case Container.Mode.Stretch =>
                math.round((space / stretch) * factor)
          val childRect = IRect.makeXYWH(rect.getLeft + width, rect.getTop, math.max(0, childWidth), math.max(0, rect.getHeight))
          child.instance.draw(child.item)(ctx, childRect, canvas) *> F.pure(width + childWidth)

        }
      } yield ()
      
    }
  }
}
