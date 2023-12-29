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
          centerButton = center[IO](button)
          paragraph <- Paragraph[IO]("hi guys2hi gguys 3 eggg eggggggggggggggggggggg")
          row = Column[IO](Child[IO].hug(centerButton), Child[IO].hug(paragraph))
          theme <- Theme.defaultTheme[IO](row, 12)
        } yield theme
      }
  
}

