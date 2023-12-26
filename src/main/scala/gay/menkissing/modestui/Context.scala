package gay.menkissing.modestui

import io.github.humbleui.jwm.Window
import cats.effect.*
import io.github.humbleui.types.IPoint
import scala.reflect.ClassTag

enum Value {
    case VInt(n: Long)
    case VDouble(n: Double)
    case VBoolean(b: Boolean)
    case VString(s: String)
    case VObject(o: AnyRef)
}
// Context of shit???
// I fucking hate this : (
// This is redone every draw call (?)

object Context {
    import Value.*
    val hovered = "hovered"
    val active = "active"
    val window = "window"
    val mousePos = "mousePos"
    val scale = "scale"

    def apply(window: Window, scale: Double, mousePos: IPoint) =
        new Context(Map(
            this.window -> VObject(window),
            this.scale -> VDouble(scale),
            this.mousePos -> VObject(mousePos) 
            ))
}
case class Context(
    extras: scala.collection.immutable.Map[String, Value]
  ) {
    import Value.*

    def window = getObj[Window](Context.window).get
    def mousePos = getObj[IPoint](Context.mousePos).get
    def scale = getDouble(Context.scale).get

    def getVal(s: String): Option[Value] = extras.get(s)
    def setVal(s: String, v: Value): Context = copy(extras = extras.updated(s, v))

    def getInt(s: String): Option[Long] = getVal(s).flatMap {
        case VInt(n) => Some(n)
        case _ => None
    }
    def setInt(s: String, n: Long): Context = setVal(s, VInt(n))
    def getDouble(s: String): Option[Double] = getVal(s).flatMap {
        case VDouble(n) => Some(n)
        case VInt(n) => Some(n)
        case _ => None
    }
    def setDouble(s: String, n: Double): Context = setVal(s, VDouble(n))
    def getBool(s: String): Option[Boolean] = getVal(s).flatMap {
        case VBoolean(b) => Some(b)
        case _ => None
    }
    def setBool(s: String, b: Boolean): Context = setVal(s, VBoolean(b))
    def getString(s: String): Option[String] = getVal(s).flatMap {
        case VString(v) => Some(v)
        case _ => None
    }
    def setString(s: String, v: String): Context = setVal(s, VString(v))
    def getObj[T <: AnyRef](s: String)(using Tag: ClassTag[T]): Option[T] = getVal(s).flatMap {
        case VObject(o) => Tag.unapply(o)
        case _ => None
    }
    def setObj(s: String, v: AnyRef): Context = setVal(s, VObject(v))

    def merge(other: Map[String, Value]) = 
        // ????
        // test me!
        copy(extras = extras ++ other)
    def merge(other: Context) = other.copy(extras = extras ++ other.extras)
}


