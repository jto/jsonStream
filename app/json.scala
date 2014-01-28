package json

object Json {

	import scalaz.stream._
  import Process._
	import scalaz.concurrent.Task

	import play.api.data.mapping._
	import play.api.libs.json._

  sealed trait J
  import play.api.data.mapping.{ PathNode => N, KeyPathNode => KPN }

  def up(p: Path): Path = Path(p.path.dropRight(1))
  def up(j: J): Path = j match {
    case Value(p, _) => up(p)
  }

  class Field(val path: Path, val node: N) extends J {
    override def toString = s"Field($path, $node)"
  }
  object Field {
    def apply(j: J, node: N) = j match {
      case Obj(p) => new Field(p, node)
      case Value(p, _) => new Field(up(p), node)
    }
    def unapply(f: Field) = Some((f.path, f.node))
  }
  class Obj(val path: Path) extends J {
    override def toString = s"Obj($path)"
  }
  object Obj {
    def apply(p: Path) = new Obj(p)
    def apply(j: J) = j match {
      case Field(path, node) => new Obj(path \ node)
      case Value(path, _) => new Obj(Path(path.path.drop(1)))
    }
    def unapply(v: Obj) = Some(v.path)
  }
  class Value(val path: Path, val js: JsValue) extends J {
    override def toString = s"Value($path, $js)"
  }
  object Value {
    def apply(j: J, js: JsValue) = j match {
      case Field(path, node) => new Value(path \ node, js)
    }
    def unapply(v: Value) = Some((v.path, v.js))
  }

	def test: Process[Task, (Path, JsValue)] = {
		import com.fasterxml.jackson.core.{ JsonFactory, JsonToken, JsonParser }
    import com.fasterxml.jackson.databind.node._
    import com.fasterxml.jackson.databind.ObjectMapper

		val mapper = new ObjectMapper
    val jsonF = mapper.getJsonFactory
  	val jp = jsonF.createJsonParser("""{ "int": 42, "string": "foo" }""")


    jp.nextToken()
    def step(j: J): Task[J] = Task.delay {
      val token = jp.nextToken()
      if(token != null) {
        import JsonToken._

        println(s"$j -> $token")

        token match {
          case n if n == VALUE_STRING =>
            Value(j, JsString(jp.getText()))
          case n if n == VALUE_NUMBER_FLOAT || n == VALUE_NUMBER_INT =>
            Value(j, JsNumber(jp.getDecimalValue()))
          case n if n == VALUE_NULL =>
            Value(j, JsNull)
          case n if n == VALUE_FALSE || n == VALUE_TRUE =>
            Value(j, JsBoolean(jp.getBooleanValue()))

          case n if n == END_ARRAY => j
          case n if n == START_OBJECT => Obj(j)
          case n if n == END_OBJECT => Obj(up(j))
          case n if n == FIELD_NAME => Field(j, KPN(jp.getText))
          case n if n == NOT_AVAILABLE => j
          case n if n == START_ARRAY => j
          case n if n == START_OBJECT => j
          case n if n == VALUE_EMBEDDED_OBJECT => j
        }

      } else throw End
    }

    def go(state: J, step: J => Task[J], onExit: Process[Task, (Path, JsValue)]): Process[Task, (Path, JsValue)] =
      await(step(state))({
        case s@Value(p, v) => emit(p -> v) ++ go(s, step, onExit)
        case s => go(s, step, onExit)
      }, onExit, onExit)

    await(Task.delay(jp))(r => {
      val onExit = Process.suspend(eval(Task.delay(jp.close())).drain)
      go(Obj(Path), step, onExit)
    }, halt, halt)

  }
}