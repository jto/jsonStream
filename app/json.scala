package json

object Parser {
  import scalaz.stream.Process
  import Process._

  import play.api.data.mapping.Path
  import play.api.libs.json.JsValue

  object Tokens {
    sealed trait Token
    case object SObj extends Token
    case object EObj extends Token
    case object SArr extends Token
    case object EArr extends Token

    case class Name(name: String) extends Token
    case class Value(js: JsValue) extends Token
  }
  import Tokens._

  def parseError(e: String) = Process.fail(new RuntimeException(e))

  private def up(p: Path): Path = Path(p.path.dropRight(1))

  // def pos(p: Process1[Token, Token]) = Process.receive1[(Path, Token), (Path, Token)] {
  //   case (path, Field(name)) => p.map(t => (path \ name, t) )
  //   case (path, EObj) if path == Path => throw End
  //   case (path, EObj) => p.map(t => (up(path), t) )
  //   case x => Process.emit(x)
  // }

  val value = Process.receive1[Token, Token]{
    case t@Value(_) => Process.emit(t)
  }

  val name: Process1[Token, Token] = Process.receive1[Token, Token] {
    case t@Name(_) => Process.emit(t)
  }

  val sobj = Process.receive1[Token, Token] {
    case SObj => Process.emit(SObj)
  }

  val eobj = Process.receive1[Token, Token] {
    case EObj => Process.emit(EObj)
  }

  val sarr = Process.receive1[Token, Token] {
    case SArr => Process.emit(SArr)
  }

  val earr = Process.receive1[Token, Token] {
    case EObj => Process.emit(EObj)
  }

  val field = name ++ json
  val obj = sobj ++ repeat(field) ++ eobj
  val arr = sarr ++ repeat(json) ++ earr
  val json: Process1[Token, Token] = arr orElse obj orElse value

}

object Json {

  import scalaz.concurrent.Task
  import scalaz.stream._
  import Process._

  import play.api.data.mapping.Path
  import play.api.libs.json.{ JsString, JsValue, JsNumber }

  import Parser.Tokens._

  def tokenise(json: String): Process[Task, Token] = {
    import play.api.data.mapping.{ PathNode => N, KeyPathNode => KPN }
    import play.api.libs.json._

    import com.fasterxml.jackson.core.{ JsonFactory, JsonToken, JsonParser }
    import com.fasterxml.jackson.databind.node._
    import com.fasterxml.jackson.databind.ObjectMapper

    val mapper = new ObjectMapper
    val jsonF = mapper.getJsonFactory
    val jp = jsonF.createJsonParser(json)

    io.resource(Task.delay(jp))(src => Task.delay(src.close)) { src =>
      jp.nextToken()
      Task.delay {
        val jtoken = jp.nextToken()
        if(jtoken != null) {
          import JsonToken._

          jtoken match {
            case n if n == VALUE_STRING => Value(JsString(jp.getText()))
            case n if n == VALUE_NUMBER_FLOAT || n == VALUE_NUMBER_INT => Value(JsNumber(jp.getDecimalValue()))
            case n if n == VALUE_NULL => Value(JsNull)
            case n if n == VALUE_FALSE || n == VALUE_TRUE => Value(JsBoolean(jp.getBooleanValue))
            case n if n == END_ARRAY => EArr
            case n if n == START_OBJECT => SObj
            case n if n == END_OBJECT => EObj
            case n if n == FIELD_NAME => Name(jp.getText)
            case n if n == NOT_AVAILABLE => SArr
            case n if n == START_ARRAY => SArr
            case n if n == START_OBJECT => SObj
            case n if n == VALUE_EMBEDDED_OBJECT => ???
          }

        } else throw End
      }
    }
  }

  def parser: Process.Process1[Token, (Path, JsValue)] = Process.receive1[Token, (Path, JsValue)] {
    case s@Value(v) => emit(Path -> v) ++ parser
    case _ => parser
  }

}