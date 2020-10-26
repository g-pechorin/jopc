package peterlavalle.jopc

import org.json.{JSONArray, JSONObject}

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

object JOPC extends App {

	import Result._

	sealed trait ObjectParser[T] extends ValueParser[T] {
		override val parseValue = {
			case json: JSONObject =>
				apply(json)
		}

		final def apply(json: JSONObject): Result[T] =
			parse(Set(), json)

		private[JOPC] def parse(seen: Set[String], json: JSONObject): Result[T]
	}

	implicit class ValueRefParser[V <: AnyRef](p: ValueParser[V]) {
		def ? : ValueParser[V] =
			sys.error("add the/a nullable parser")
	}

	trait ValueParser[T] {

		def parseValue: AnyRef => Result[T]

		def !![O](f: T => O): ValueParser[O] = this ! (v => Right(f(v)))

		def ![O](f: T => Result[O]): ValueParser[O] = {
			val from = this

			new ValueParser[O] {
				override def parseValue: AnyRef => Result[O] = {
					value: AnyRef =>
						from.parseValue(value).flatMap(f)
				}
			}
		}

		def |[O, R](r: ValueParser[R]): ValueParser[O] =
			???

		def * : ArrayParser[T] = this * 0

		def *(max: Int): ArrayParser[T] = this * (0 -> max)

		def *(minMax: (Int, Int)): ArrayParser[T] = ArrayParser(minMax._1, minMax._2, this)
	}

	trait FieldParser[T] {
		def key: String

		def value: ValueParser[T]

		def map[O](f: T => O): ObjectParser[O]

		def flatMap[O](f: T => ObjectParser[O]): ObjectParser[O]
	}

	case class ArrayParser[T] private(min: Int, max: Int, value: ValueParser[T]) extends ValueParser[List[T]] {
		def apply(json: JSONArray): Result[List[T]] = {
			(0 until json.length())
				.toStream
				.map(json.get)
				.foldLeft(Result(List[T]())) {
					case (left, json) =>
						left.flatMap {
							list: List[T] =>
								value.parseValue(json) match {
									case Result(value) =>
										Result(list :+ value)
									case failure =>
										failure.asInstanceOf
								}
						}
				}
		}

		def parseValue = {
			case null =>
				???
		}
	}

	case class StringFieldParser[T](key: String, value: ValueParser[T]) extends FieldParser[T] {

		override def map[O](f: T => O): ObjectParser[O] = Pure(f)

		override def flatMap[O](f: T => ObjectParser[O]): ObjectParser[O] = Bind(f)

		case class Pure[O](f: T => O) extends ObjectParser[O] {
			override protected[JOPC] def parse(seen: Set[String], json: JSONObject): Result[O] = {
				require(json.keySet().toSet.forall(seen + key))
				value.parseValue(json.get(key)).map(f)
			}
		}

		case class Bind[O](f: T => ObjectParser[O]) extends ObjectParser[O] {
			override protected[JOPC] def parse(seen: Set[String], json: JSONObject): Result[O] = {
				require(!seen(key))
				value.parseValue(json.get(key)).map(f).flatMap((_: ObjectParser[O]) parse(seen + key, json))
			}
		}

	}

	object ValueParser {

		val AtomicDouble: ValueParser[Double] =
			AtomicString ! {
				value: String =>
					val toDouble: Double = value.toDouble
					if (toDouble.toString == value)
						Right(toDouble)
					else
						UnstableAtomicValue(
							value,
							"double"
						)
			}
		val AtomicFloat: ValueParser[Float] = AtomicDouble !! ((_: Double).toFloat)
		val AtomicBoolean: ValueParser[Boolean] =
			AtomicString !! ((_: String).toLowerCase()) ! {
				case "1" | "t" | "true" =>
					Right(true)
				case "0" | "f" | "false" =>
					Right(true)
				case what =>
					Failure(s"unexpected boolean `$what`")
			}

		case object AtomicString extends ValueParser[String] {
			val parseValue = {
				case string: String => Result(string)
			}
		}

		case object AtomicInt extends ValueParser[Int] {
			val parseValue = {
				case value: String =>
					val toInt: Int = value.toInt
					if (toInt.toString == value)
						Right(toInt)
					else
						UnstableAtomicValue(
							value,
							"int"
						)

				case int: java.lang.Integer =>
					val i: Int = int
					Right(i)
			}
		}

	}

	implicit class PiStringKey(key: String) {
		def %[T](value: ValueParser[T]): FieldParser[T] = StringFieldParser(key, value)
	}

}
