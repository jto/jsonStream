package json

object Json {

	import scalaz.stream._
  import Process._
	import scalaz.concurrent.Task

	import play.api.data.mapping._
	import play.api.libs.json._

	def test: Process[Task, (Path, JsValue)] = {
		import com.fasterxml.jackson.core.{ JsonFactory, JsonToken, JsonParser }
    import com.fasterxml.jackson.databind.node._
    import com.fasterxml.jackson.databind.ObjectMapper

		val mapper = new ObjectMapper
    val jsonF = mapper.getJsonFactory
  	val jp = jsonF.createJsonParser("""{ "int": 42, "string": "foo" }""")

    sealed trait In
    case class El(path: Path, js: JsValue) extends In
    case class P(path: Path) extends In {
      def has(js: JsValue) = El(path, js)
    }

    def step(path: Path): Task[In] = Task.delay {
      val token = jp.nextToken()
      if(token != null) {
        import JsonToken._

        val p  = P(path)
        token match {
          case n if n == VALUE_STRING =>
            p has JsString(jp.getText())
          case n if n == VALUE_NUMBER_FLOAT || n == VALUE_NUMBER_INT =>
            p has JsNumber(jp.getDecimalValue())
          case n if n == VALUE_NULL =>
            p has JsNull
          case n if n == VALUE_FALSE || n == VALUE_TRUE =>
            p has JsBoolean(jp.getBooleanValue())

          case n if n == END_ARRAY => p
          case n if n == START_OBJECT => p
          case n if n == END_OBJECT => p
          case n if n == FIELD_NAME => P(p.path \ jp.getText)
          case n if n == NOT_AVAILABLE => p
          case n if n == START_ARRAY => p
          case n if n == START_OBJECT => p
          case n if n == VALUE_EMBEDDED_OBJECT => p
        }

      } else throw End
    }

    def go(path: Path, step: Path => Task[In], onExit: Process[Task, (Path, JsValue)]): Process[Task, (Path, JsValue)] =
      await(step(path))({
        case El(p, v) => emit(p -> v) ++ go(p, step, onExit)
        case P(p) => go(p, step, onExit)
      }, onExit, onExit)

    await(Task.delay(jp))(r => {
      val onExit = Process.suspend(eval(Task.delay(jp.close())).drain)
      go(Path, step, onExit)
    }, halt, halt)

  }
}