package peterlavalle.jopc

import org.json.JSONObject
import org.scalatest.funsuite.AnyFunSuite
import peterlavalle.jopc.JOPC._
import peterlavalle.jopc.Result.Result

class SimpleTest extends AnyFunSuite {
	test("hey you") {
		case class Foo(h: String, y: Int)

		val parser: ObjectParser[Foo] =
			for {
				hey <- "hey" % ValueParser.AtomicString
				you <- "you" % ValueParser.AtomicInt
			} yield {
				Foo(hey, you)
			}

		val actual: Result[Foo] = parser(new JSONObject().put("hey", "you").put("you", 1287))

		assert(
			actual == Result(Foo("you", 1287))
		)
	}
}
