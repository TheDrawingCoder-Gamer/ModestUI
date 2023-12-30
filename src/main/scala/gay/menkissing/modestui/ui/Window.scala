package gay.menkissing.modestui.ui

import io.github.humbleui.jwm.{App, MouseCursor, Platform, TextInputClient, Window as JWindow, ZOrder, Event}
import io.github.humbleui.jwm
import io.github.humbleui.jwm.skija.{LayerD3D12Skija, LayerGLSkija, LayerMetalSkija, EventFrameSkija}
import io.github.humbleui.skija.{Surface, Canvas}
import io.github.humbleui.types.{IRect, IPoint}
import gay.menkissing.modestui.*
import cats.effect.*
import cats.effect.std.*
import cats.effect.syntax.all.*
import cats.implicits.*
import scala.concurrent.ExecutionContext
object UIThreadDispatchEC extends ExecutionContext {
  def execute(r: Runnable): Unit = App._nRunOnUIThread(r)
  def reportFailure(t: Throwable): Unit = {
    println(t)
    t.printStackTrace()
  }
}
object Window {
  def scale[F[_]](window: JWindow)(using F: Async[F]): F[Float] =
    F.delay { window.getScale } 
  def debugErr[F[_]](err: Throwable)(using F: Async[F]): F[Unit] =
    F.delay { println(err.getMessage) }
    *> F.delay { err.printStackTrace() }
    *> F.pure(())

  // Note: requires running app
  def make[F[_]](onCloseRequest: Option[JWindow => F[Unit]], 
               onClose: Option[F[Unit]],
               onScreenChange: Option[JWindow => F[Unit]],
               onResize: Option[JWindow => F[Unit]],
               onPaint: Option[(JWindow, Canvas) => F[Unit]],
               onEvent: Option[(JWindow, events.MEvent) => F[Unit]],
              )(using F: Async[F]): Resource[F, JWindow] = {
                for {
                  // Are you even captured? 
                  dispatcher <- Dispatcher.sequential[F]
                  window <- Resource.make(F.delay { App.makeWindow() })(window => F.delay { window.close() })
                  layer <- F.delay {
                    Platform.CURRENT match {
                      case Platform.WINDOWS => new LayerD3D12Skija()
                      case Platform.MACOS => new LayerMetalSkija()
                      case Platform.X11 => new LayerGLSkija()
                      case Platform.WAYLAND => new LayerGLSkija()
                    }
                  }.toResource
                  goodListener = (event: Event) => 
                    for {
                      _ <- event match {
                        case _: EventFrameSkija => F.pure(())
                        case _: jwm.EventFrame => F.pure(())
                        case _ =>
                          onEvent.traverse(_(window, events.MEvent.fromJWM(event))).void
                      }
                      _ <- event match {
                        case _: jwm.EventWindowCloseRequest =>
                          onCloseRequest.traverse(_(window)).void
                        case _: jwm.EventWindowClose =>
                          onClose.traverse(identity).void
                        case _: jwm.EventWindowScreenChange =>
                          onScreenChange.traverse(_(window)).void
                          
                        case e: EventFrameSkija =>
                          // yay?
                          onPaint.traverse { onPaint =>
                            for {
                              isNull <- F.delay { io.github.humbleui.skija.impl.Native.getPtr(e._surface) == 0 }
                              _ <- 
                                if (!isNull)
                                  for {
                                    canvas <- F.delay { e.getSurface.getCanvas }
                                    layer <- F.delay { canvas.save() }
                                    _ <-
                                      Resource.makeCase[F, Unit](F.pure {()}) { (_, exitCase) =>
                                        (exitCase match {
                                          case Resource.ExitCase.Errored(_) => F.delay { canvas.clear(0xFFCC3333) }
                                          case _ => F.pure(())
                                        }) *> F.delay { canvas.restoreToCount(layer) }
                                      }.use(_ => onPaint(window, canvas))
                                  } yield ()
                                else
                                  F.pure(())

                            } yield ()
                          }.void
                        case _: jwm.EventWindowResize =>
                          onResize.traverse(_(window)).void *> F.delay { window.requestFrame() }
                        case _ =>
                          F.pure(())
                      }
                    } yield ()
                  listener <-
                    F.delay {
                      (event: Event) => {
                        dispatcher.unsafeRunAndForget(goodListener(event))
                      }
                    }.toResource
      
                  _ <- F.delay { window.setLayer(layer) }.toResource
                  _ <- F.delay { window.setEventListener(new java.util.function.Consumer[Event] { override def accept(it: Event) = listener(it) }) }.toResource
                  // TODO: Text input?

                } yield window 
              }.evalOn(UIThreadDispatchEC)
  // TRUE!
  // do not let this be off of the UI thread or i will hurt you
  class CurriedApply[F[_]] private[modestui] (val underlying: Boolean = true) extends AnyVal {
    def apply[T](exitOnClose: Boolean = true,
                  title: String = "ModestUI",
                  width: Int = 800,
                  height: Int = 600,
                  screen: Int = -1,
                  bgColor: Int = 0xFFF6F6F6
    )(daApp: Resource[F, T])(using F: Async[F], C: Component[F, T]): Resource[F, JWindow] = 
      for { 
        app <- daApp
        mousePos <- Ref[F].of(IPoint(0, 0)).toResource
        window <- make[F](Some(it => F.delay { it.close()}), Option.when(exitOnClose)(F.delay { App.terminate() }), None, None, 
          Some((window, canvas) => {
            for {
              _ <- F.delay { canvas.clear(bgColor) }
              bounds <- F.delay { window.getContentRect }
              mPos <- mousePos.get
              dascale <- scale[F](window)
              ctx = Context(window, dascale, mPos)
              _ <- app.draw(ctx, IRect.makeXYWH(0, 0, bounds.getWidth, bounds.getHeight), canvas)
              
            } yield ()
          }),
        // Request frame uses App.runOnUIThread, which I know is stack unsafe
        Some((window: JWindow, event: events.MEvent) =>
            if (window.isClosed)
              F.unit;
            else  {
              for {
                _ <- event match {
                  case e: events.MMouseEvent =>
                    mousePos.set(IPoint(e.x, e.y))
                  case _ => F.pure(())
                }
                mPos <- mousePos.get
                dascale <- scale[F](window)
                ctx = Context(window, dascale, mPos)
                needsRedraw <- app.event(ctx, event)
                _ <- F.whenA(needsRedraw) { F.delay { window.requestFrame() } }
                // _ <- F.delay { window.requestFrame() }
              } yield ()
            })).evalTap { window =>
          (for {
            _ <- F.delay { window.setWindowSize(width, height) }
            // TODO: position
            _ <- F.delay { window.setTitle(title) }
            _ <- F.delay { window.setVisible(true) }
          } yield ()).evalOn(UIThreadDispatchEC)
        }
      } yield window

  }
  def apply[F[_]](using F: Async[F]): CurriedApply[F] =
    new CurriedApply[F]

}
