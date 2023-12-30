package gay.menkissing.modestui.ui

import cats.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.all.*

import gay.menkissing.modestui.*

import io.github.humbleui.types.{IPoint, IRect}
import io.github.humbleui.skija.Canvas
import io.github.humbleui.jwm.Event
import io.github.humbleui.jwm

class Hoverable[F[_], C](val onHover: Option[events.MEvent => F[Unit]], val onHoverOut: Option[events.MEvent => F[Unit]], val child: C,
  val childRect: Ref[F, IRect], val hovered: Ref[F, Boolean]) 
{
    def context(context: Context)(using F: Async[F]): F[Context] =
      for {
        hover <- hovered.get
      } yield context.setBool(Context.hovered, hover)
}

object Hoverable {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[C](child: C, onHover: events.MEvent => F[Unit] = null, onHoverOut: events.MEvent => F[Unit] = null)(using F: Async[F], C: Component[F, C]) =
      for {
        rectRef <- Ref[F].of(IRect(0, 0, 0, 0))
        hovered <- Ref[F].of(false)
      } yield new Hoverable(Option(onHover), Option(onHoverOut), child, rectRef, hovered)
  }
  
  def apply[F[_]] = new BuildOps[F]
}
class Clickable[F[_], C](val onClick: Option[events.MEvent => F[Unit]], val onClickCapture: Option[events.MEvent => F[Unit]], val child: C,
  val childRect: Ref[F, IRect], val hovered: Ref[F, Boolean], val pressed: Ref[F, Boolean], val clicks: Ref[F, Int],
  val lastClick: Ref[F, Long]) {
    def context(context: Context)(using F: Async[F]): F[Context] =
      for {
        hover <- hovered.get
        press <- pressed.get 
      } yield context.setBool(Context.hovered, hover).setBool(Context.active, press && hover)
  }

object Clickable {
  // TRUE!
  case class BuildOps[F[_]](underlying: Boolean = true) extends AnyVal {
    def apply[C](child: C, onClick: events.MEvent => F[Unit] = null, onClickCapture: events.MEvent => F[Unit] = null)
    (using F: Async[F], C: Component[F, C]): Resource[F, Clickable[F, C]] =
      (for {
        rectRef <- Ref[F].of(IRect(0, 0, 0, 0))
        hovered <- Ref[F].of(false)
        pressed <- Ref[F].of(false)
        clicks <- Ref[F].of(1)
        lastClick <- Ref[F].of(1l)
      } yield new Clickable(Option(onClick), Option(onClickCapture), child, rectRef, hovered, pressed, clicks, lastClick)).toResource
  }
  def apply[F[_]] = new BuildOps[F]
}
given clickable_Component[F[_], C](using F: Async[F], C: Component[F, C]): Component[F, Clickable[F, C]] with
  extension (self: Clickable[F, C]) {
    def measure(ctx: Context, rect: IPoint): F[IPoint] = 
      self.context(ctx).flatMap(ctx => self.child.measure(ctx, rect))
    def draw(ctx: Context, rect: IRect, canvas: Canvas): F[Unit] =
      for {
        _ <- self.childRect.set(rect)
        _ <- self.hovered.set(rectContains(rect, ctx.mousePos))
        dactx <- self.context(ctx)
        _ <- self.child.draw(dactx, rect, canvas)
      } yield ()
    def map(ctx: Context, cb: Instance[[X] =>> Component[F, X]] => F[Unit]): F[Unit] = self.context(ctx).flatMap { ctx =>
      cb(Instance(self)(using this)) *> self.child.map(ctx, cb)
    }
    def event(ctx: Context, event: events.MEvent): F[Boolean] = 
      for {
        _ <- F.whenA(event.isInstanceOf[jwm.EventMouseMove])(self.clicks.set(0) *> self.lastClick.set(0))
        res <-
          eagerOrF[F](
            {
              (event match {
                case e: events.MMouseEvent =>
                  Some((e.x, e.y))
                case _ =>
                  None
              }).traverse { case (x, y) =>
                val point = IPoint(x, y)
                for {
                  childRect <- self.childRect.get
                  hovered = rectContains(childRect, point)
                  curHovered <- self.hovered.get
                  _ <- F.whenA(hovered != curHovered)(self.hovered.set(hovered))
                } yield (hovered != curHovered)
              }.map(_.getOrElse(false))
            },
            {
              for {
                hovered <- self.hovered.get
                pressed <- self.pressed.get
                newPressed =
                  event match {
                    case e: events.MEventMouseButton =>
                      if (e.isPressed)
                        hovered 
                      else
                        false 
                    case _ => 
                      pressed
                  }
                clicked = pressed && !newPressed && hovered 
                now <- F.delay { System.currentTimeMillis }
                _ <-
                  F.whenA(clicked) {
                    for {
                      lastClickie <- self.lastClick.get
                      _ <- F.whenA((now - lastClickie) > doubleClickThresholdMS)(self.clicks.set(0))
                      _ <- self.clicks.update(_ + 1)
                      _ <- self.lastClick.set(now)
                    } yield ()
                  }
                res <- 
                  for {
                    _ <-
                      // TODO: Clicks???
                      F.whenA(clicked)(self.onClickCapture.traverse(_(event)))
                    dactx <- self.context(ctx)
                    res <- 
                      lazyOrF[F](
                        self.child.event(dactx, event),
                        eagerOrF[F](
                          if (clicked && !self.onClick.isEmpty) 
                            self.onClick.get(event) *> F.pure(true)
                          else 
                            F.pure(false),
                          if (pressed != newPressed)
                            self.pressed.set(newPressed) *> F.pure(true)
                          else 
                            F.pure(false)
                          )
                        )
                  } yield res
              } yield res

            }
            )
      } yield res
  }

