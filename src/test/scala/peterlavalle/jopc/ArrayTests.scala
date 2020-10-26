package peterlavalle.jopc

import org.json.{JSONArray, JSONObject}
import org.scalatest.funsuite.AnyFunSuite
import peterlavalle.jopc.JOPC._
import peterlavalle.jopc.Result.Result

class ArrayTests extends AnyFunSuite {

	test("list") {
		case class Foo(h: String, y: Int)

		val parser: ArrayParser[Foo] =
			(for {
				hey <- "hey" % ValueParser.AtomicString
				you <- "you" % ValueParser.AtomicInt
			} yield {
				Foo(hey, you)
			}).*

		val actual: Result[List[Foo]] =
			parser {
				new JSONArray()
					.put {
						new JSONObject()
							.put("hey", "you")
							.put("you", 1287)
					}
					.put {
						new JSONObject()
							.put("hey", "explore")
							.put("you", -123)
					}
					.put {
						new JSONObject()
							.put("hey", "summer")
							.put("you", 0)
					}
			}

		assert(
			actual == Result(
				List(
					Foo("you", 1287),
					Foo("explore", -123),
					Foo("summer", 0),
				)
			)
		)
	}
}
