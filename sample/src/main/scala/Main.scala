import gay.menkissing.modestui.ui.{*, given}
import gay.menkissing.modestui.App
import cats.effect.IOApp
import cats.effect.*
import cats.effect.syntax.all.*
import cats.implicits.*
object Main extends IOApp.Simple {
  def baseRun = 
      Window[IO]() {
        for {
          label <- Label[IO]("hi guys")
          center <- center[IO, Label[IO]](label)
        } yield center
      }
  def run = 
    App.start[IO](baseRun.void).useForever
  
}

