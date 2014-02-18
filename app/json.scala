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

  def parser(expected: String)(f: PartialFunction[Token, Process1[Token, Token]]) =
    receive1{ (x: Token) => println(x); f(x) } // onFailure Process.fail(new RuntimeException(s"expected: $expected"))

  val value = parser("value"){ case t@Value(_) => emit(t) }
  val name = parser("name") { case t@Name(_) => emit(t) }
  val sobj = parser("sobj") { case SObj => emit(SObj) }
  val eobj = parser("eobj") { case EObj => emit(EObj) }
  val sarr = parser("sarr") { case SArr => emit(SArr) }
  val earr = parser("earr") { case EObj => emit(EObj) }

  lazy val field = name ++ value
  lazy val obj = sobj ++ repeat(field) ++ eobj
  lazy val arr = sarr ++ repeat(json) ++ earr
  lazy val json: Process1[Token, Token] = value onFailure arr onFailure obj

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
            case n if n == NOT_AVAILABLE => ???
            case n if n == START_ARRAY => SArr
            case n if n == VALUE_EMBEDDED_OBJECT => ???
          }

        } else throw End
      }
    }
  }

  val withPath = {
    def up(path: Path): Path = Path(path.path.dropRight(1))

    def trackPath(p: Path): Process.Process1[Token, (Path, Token)]  = receive1 {
      case n@Name(name) => trackPath(p \ name)
      case EObj if p == Path => throw End
      case EObj => trackPath(up(p))
      case t => emitSeq(Seq(p -> t), trackPath(p))
    }

    trackPath(Path)
  }

  // val parse = Parser.json |> withPath


}
