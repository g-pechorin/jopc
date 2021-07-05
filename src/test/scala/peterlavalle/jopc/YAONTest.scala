package peterlavalle.jopc

import java.io.InputStream

import org.json.JSONObject
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}

class YAONTest extends AnyFunSuite {

	def testResource[O](suffix: String)(act: BufferedSource => O): O = {

		val name: String = getClass.getSimpleName + suffix
		val stream: InputStream = getClass.getResourceAsStream(name)

		require(null != stream, s"no stream from resource $name")

		val source: BufferedSource =
			Source
				.fromInputStream(
					stream
				)

		try {
			act(source)
		} finally {
			source.close()
		}
	}

	lazy val yaonString: String = {
		testResource(".frame.yaon") {
			(_: BufferedSource).mkString
		}
	}

	test("from text") {


		// sanity check (remember - these puppers don't do equality)
		assert(
			js.toString == new JSONObject(js.toString).toString,
			"sanity check (remember - these puppers don't do equality)"
		)


		val (tag, json) = YAON(ya.split("[\t \r\n]*\n"))

		assert("thing" == tag)

		assert(js.toString == json.toString)

		assert(YAON("thing", js) == YAON("thing", json))
	}

	def ya =
		"""
			|thing
			|bar: fourteen
			|foo: -9
			""".stripMargin

	def js =
		new JSONObject()
			.put("foo", -9)
			.put("bar", "fourteen")

	def jsonData: JSONObject =
		testResource(".frame.json")(
			(src: BufferedSource) =>
				new JSONObject(src.mkString)
		)

	test("parse resource") {

		val (tag, yaonData) = YAON(yaonString)

		require("json" == tag)

		if (!(yaonData deepEqu jsonData))
			fail("the parsed things were not equal")
	}

	test("encode simple yaon") {
		assert(
			YAON("thing", js)
				.foldLeft("")(_ + _ + "\n")
				.trim == ya.trim
		)
	}

	/**
	 * test YAON by encoding a more complex JSONObject
	 */
	test("encode resource yaon") {
		assert(
			YAON("json", jsonData)
				.foldLeft("")((_: String) + (_: String) + "\n")
				.trim == yaonString.trim
		)
	}

}
