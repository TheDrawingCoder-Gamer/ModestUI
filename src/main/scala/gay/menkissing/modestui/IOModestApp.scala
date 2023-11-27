package gay.menkissing.modestui

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.github.humbleui.jwm.{Window, App as JApp}
/*
 * A simple app with only 1 window : )
 */
trait IOModestApp extends IOApp.Simple {
  def render: Resource[IO, Any]

  def run = 
    App.start(render.void).use_
}
