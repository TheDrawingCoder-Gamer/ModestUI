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
          center <- center[IO, Label[IO]](label)
        } yield center
      }
  
}

