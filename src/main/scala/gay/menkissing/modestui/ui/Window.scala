package gay.menkissing.modestui.ui

import io.github.humbleui.jwm.{App, MouseCursor, Platform, TextInputClient, Window as JWindow, ZOrder, Event}
import io.github.humbleui.jwm
import io.github.humbleui.jwm.skija.{LayerD3D12Skija, LayerGLSkija, LayerMetalSkija, EventFrameSkija}
import io.github.humbleui.skija.{Surface, Canvas}
import io.github.humbleui.types.IRect
import gay.menkissing.modestui.Component
import cats.effect.*
import cats.effect.std.*
import cats.effect.syntax.all.*
import cats.implicits.*
import scala.concurrent.ExecutionContext
object UIThreadDispatchEC extends ExecutionContext {
  def execute(r: Runnable): Unit = App.runOnUIThread(r)
  def reportFailure(t: Throwable): Unit = {
    println(t)
    t.printStackTrace()
  }
}
object Window {
  // Note: requires running app
  def make[F[_]](onCloseRequest: Option[JWindow => F[Unit]], 
               onClose: Option[F[Unit]],
               onScreenChange: Option[JWindow => F[Unit]],
               onResize: Option[JWindow => F[Unit]],
               onPaint: Option[(JWindow, Canvas) => F[Unit]],
               onEvent: Option[(JWindow, Event) => F[Unit]],
              )(using F: Async[F]): Resource[F, JWindow] = {
                for {
                  dispatcher <- Dispatcher.sequential[F]
                  window <- F.delay { App.makeWindow() }.toResource.evalOn(UIThreadDispatchEC)
                  layer <- F.delay {
                    Platform.CURRENT match {
                      case Platform.WINDOWS => new LayerD3D12Skija()
                      case Platform.MACOS => new LayerMetalSkija()
                      case Platform.X11 => new LayerGLSkija()
                    }
                  }.toResource
                  listener =
                    (event: Event) => {
                      // TODO: don't raw dog these
                      // this will crash if any of these functions throw
                      if (!event.isInstanceOf[EventFrameSkija]) {
                        onEvent match {
                          case Some(f) => dispatcher.unsafeRunSync(f(window, event))
                          case _ => ()
                        }
                      }
                      event match {
                        case _: jwm.EventWindowCloseRequest =>
                          onCloseRequest.foreach(it => dispatcher.unsafeRunSync(it(window)))
                        case _: jwm.EventWindowClose =>
                          onClose.foreach(it => dispatcher.unsafeRunSync(it))
                        case _: jwm.EventWindowScreenChange =>
                          onScreenChange.foreach(it => dispatcher.unsafeRunSync(it(window)))
                        case _: jwm.EventWindowResize =>
                          onResize.foreach(it => dispatcher.unsafeRunSync(it(window)))
                        case e: EventFrameSkija =>
                          // the """fun""" one
                          onPaint.foreach { onPaint =>
                            val canvas = e._surface.getCanvas
                            val layer = canvas.save()
                            try {
                              dispatcher.unsafeRunSync(onPaint(window, canvas))
                            } catch {
                                case _ => 
                                  canvas.clear(0xFFCC3333)
                            } finally {
                              canvas.restoreToCount(layer)
                            }
                          }
                        case _ => ()
                      }
                    }
                  _ <- F.delay { window.setLayer(layer) }.toResource
                  _ <- F.delay { window.setEventListener(new java.util.function.Consumer[Event] { override def accept(it: Event) = listener(it) }) }.toResource
                  // TODO: Text input?

                } yield window 
              }
  // TRUE!
  class CurriedApply[F[_]] private[modestui] (val underlying: Boolean = true) extends AnyVal {
    def apply[T](exitOnClose: Boolean = true,
                  title: String = "ModestUI",
                  width: Int = 800,
                  height: Int = 600,
                  screen: Int = -1,
                  bgColor: Int = 0xFFF6F6F6
    )(daApp: Resource[F, T])(using F: Async[F], C: Component[F, T]): Resource[F, JWindow] = 
      // forbidden exit
      for { 
        app <- daApp
        window <- make[F](Some(it => F.delay { it.close()}), Option.when(exitOnClose)(F.delay { System.exit(0) }), None, None, 
          Some((window, canvas) => {
            for {
              _ <- F.delay { canvas.clear(bgColor) }
              bounds <- F.delay { window.getContentRect }
              _ <- app.draw(IRect.makeXYWH(0, 0, bounds.getWidth, bounds.getHeight), canvas)

            } yield ()
          }), 
        Some((window, event) => app.topic.publish1(event) *> F.delay { window.requestFrame() } )).flatTap { window =>
          (for {
            _ <- F.delay { window.setWindowSize(width, height) }
            // TODO: position
            _ <- F.delay { window.setTitle(title) }
            _ <- F.delay { window.setVisible(true) }
          } yield ()).toResource
        }
      } yield window
  }
  def apply[F[_]](using F: Async[F]): CurriedApply[F] =
    new CurriedApply[F]

}
