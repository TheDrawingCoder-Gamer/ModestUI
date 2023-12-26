import gay.menkissing.modestui.ui.{*, given}
import gay.menkissing.modestui.IOModestApp
import cats.effect.IOApp
import cats.effect.*
import cats.effect.syntax.all.*
import cats.implicits.*
object Main extends IOModestApp {
  def render = 
      Window[IO]() {
        for {
          label <- Label[IO]("hi guys")
          button <- Button(IO.println("WOWIE ZOWIE"), label)
          // NEW LIFE HACK!
          center <- center[IO](button)
          theme <- Theme.defaultTheme[IO](center, 12)
        } yield theme
      }
  
}

