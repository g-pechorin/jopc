package peterlavalle.jopc

import org.json.JSONObject
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable

class OfJSONTests extends AnyFunSuite {

	import Bar._
	import Foo._


	test("foo in") {
		(0 until 40000).foreach {
			_: Int =>
				methodFoo.enJSON(methodFoo.generate())
		}
	}

	test("foo 40 in then out") {

		// generate
		val src =
			(0 until 40)
				.map(_ => methodFoo.generate())

		// encode
		val dat: immutable.Seq[JSONObject] =
			src.map(methodFoo.enJSON)


		// decode and test
		(src zip dat)
			.foreach {
				case (expected: Foo, dat) =>
					assert(methodFoo.deJSON(dat) == expected)
			}
	}

	test("foo 40,000 in then out") {

		// generate
		val src =
			(0 until 40000)
				.map(_ => methodFoo.generate())

		// encode
		val dat: immutable.Seq[JSONObject] =
			src.map(methodFoo.enJSON)


		// decode and test
		(src zip dat)
			.foreach {
				case (expected: Foo, dat) =>
					assert(methodFoo.deJSON(dat) == expected)
			}
	}


	test("bar 84 in then out") {

		// generate
		val src: Seq[Bar] =
			(0 until 84)
				.map(_ => methodBar.generate())

		// encode
		val dat =
			src.map(methodBar.enJSON)

		// decode and test
		(src zip dat).foreach {
			case (expected: Bar, dat) =>
				assert(methodBar.deJSON(dat) == expected)
		}
	}

	test("bar 1984 in then out") {

		// generate
		val src: Seq[Bar] =
			(0 until 1984)
				.map(_ => methodBar.generate())

		// encode
		val dat =
			src.map(methodBar.enJSON)

		// decode and test
		(src zip dat).foreach {
			case (expected: Bar, dat) =>
				assert(methodBar.deJSON(dat) == expected)
		}
	}
}
