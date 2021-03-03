package peterlavalle.jopc

import java.io.{File, FileWriter, Writer}

import org.json.{JSONArray, JSONObject}

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

object YAON {

	@tailrec
	def file(tag: String, file: File): JSONFile =
		if (!file.isAbsolute)
			YAON.file(tag, file.getAbsoluteFile)
		else
			new JSONFile {
				val data: JSONObject =
					if (!file.exists())
						new JSONObject()
					else {
						val bufferedSource: BufferedSource = Source.fromFile(file)
						val (loadedTag, loadedJson) =
							try {
								YAON.apply(bufferedSource.mkString)
							} finally {
								bufferedSource.close()
							}
						require(loadedTag == tag || null == loadedTag)
						loadedJson
					}

				override def apply[O](act: JSONObject => O): O =
					data.synchronized {
						val out: O = act(data)

						val parent: File = file.getParentFile.getAbsoluteFile
						require(parent.isDirectory || parent.mkdirs())

						YAON(tag, data)
							.foldLeft(new FileWriter(file): Writer)(
								(_: Writer).append(_: String).append("\n")
							)
							.close()
						out
					}
			}

	def apply(tag: String, json: JSONObject): Stream[String] = {

		//
		def compute: AnyRef => Either[String, Stream[String]] = {
			case symbol if symbol.toString.matches("-?\\w+") =>
				Left(symbol.toString)

			case child: JSONObject =>
				Right(save(child)(null))

			case text: String =>
				Left(enQuote(text))

			case float: java.lang.Float => Left(float.toString)

			case list: JSONArray =>
				Right {
					// so what we/i want here is to nest/reuse computing the/a value
					// ... but - this method don't do that ... yet
					list
						.toStreamOf(_ get _).map(compute).flatMap {
						case Left(value) =>
							Stream("-" + value)
						case Right(value) =>
							"-" #:: value.map("\t" + _)
					}
				}
		}


		def save(json: JSONObject): List[String] => Stream[String] = {

			case null =>
				save(json)(
					json.keySet().toArray
						.map { case key: String => key }
						.toList
						.sorted)

			case Nil => Stream()

			case next :: todo =>


				require(next matches "\\w+")
				val tail: Stream[String] = save(json)(todo)
				val key: String = next + ":"

				val value: AnyRef = json.get(next)


				lazy val leftValue = {
					val Left(rVal) = compute(value)
					rVal
				}

				compute(value) match {
					case Left(value) =>
						(key + " " + value) #:: tail

					case Right(subValue) =>
						key #:: subValue.map("\t" + (_: String)) ++ tail
				}

		}

		tag #:: save(json)(null)
	}

	def enQuote(src: String): String = {

		val son = new JSONObject().put("s", src).toString

		val open = "{\"s\":\""
		val close = "\"}"

		require(son.startsWith(open) && son.endsWith(close))
		require(!son.contains("\n"))

		son.drop(open.length - 1).dropRight(close.length - 1)
	}

	def apply(file: File, want: String): JSONObject = {
		val (tag, data) = YAON(file)
		require(want == tag)
		data
	}

	def apply(file: File): (String, JSONObject) = {
		val source = Source.fromFile(file)
		try {
			YAON(source.mkString)
		} finally {
			source.close()
		}
	}

	def apply(text: String): (String, JSONObject) = {
		YAON(text.split("[\r \t]*\n"))
	}

	def apply(data: Iterable[String]): (String, JSONObject) = {

		val rInteger: Regex = "(\\w+): (-?\\d+)".r
		val rString: Regex = "(\\w+): (.+)".r
		val rQuoted: Regex = "(\\w+): \"(.+)\"".r
		val rBlank: Regex = "[\t ]*".r
		val rSubValue: Regex = "(\\w+):".r


		def load(json: JSONObject): Stream[String] => JSONObject = {
			case Stream() => json
			case rBlank() #:: data =>
				load(json)(data)
			case rInteger(key, value) #:: data =>
				load(json.put(key, value.toInt))(data)

			case rQuoted(key, value) #:: data =>
				load(json.put(key, deQuote("\"" + value + "\"")))(data)

			case rString(key, value) #:: data =>
				load(json.put(key, value))(data)

			case rSubValue(key) #:: data if data.map((_: String).tail).head.startsWith("-") =>

				def loop(json: JSONArray): Stream[String] => JSONArray = {
					case Stream() => json
					case "-" #:: data =>
						loop(
							json.put(load(new JSONObject())(data.takeWhile(_ startsWith "\t").map(_.tail)))
						)(
							data.dropWhile(_ startsWith "\t")
						)

					case line #:: tail =>
						loop(json.put(deQuote(line.tail)))(tail)
				}

				load(
					json.put(
						key,
						loop(new JSONArray()) {
							data
								.takeWhile((_: String).startsWith("\t"))
								.map(_.tail)
						}
					)
				)(
					data.dropWhile(_.startsWith("\t"))
				)

			case rSubValue(key) #:: data =>
				load(
					json.put(
						key,
						load(new JSONObject())(
							data
								.takeWhile(_.startsWith("\t"))
								.map(_.tail)
						)
					)
				)(data.dropWhile(_.startsWith("\t")))
		}

		data.dropWhile((_: String).trim.isEmpty).toStream match {
			case Empty =>
				(null, new JSONObject())
			case tag #:: data =>
				tag -> load(new JSONObject())(data
					.filterNot {
						line => line.trim.startsWith(";") || line.trim == ""
					}
				)
		}
	}

	def deQuote(src: String): String = {

		val line = {
			val line = src.trim

			if (line.startsWith("\""))
				line
			else
				'"' + line + '"'
		}

		if (!(line.startsWith("\"") && line.endsWith("\"")))
			require(
				line.startsWith("\"") && line.endsWith("\""),
				s"failed to dequote `$line`"
			)

		val str: String = "{\"s\": " + line + "}"
		new JSONObject(str)
			.getString("s")
	}
}
