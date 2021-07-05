package peterlavalle.jopc

import java.nio.ByteBuffer
import java.util.Random

import org.json.{JSONArray, JSONObject}

import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * combines code-as, rand-of (... and a new editor?)
 *
 */
sealed trait member[T] extends member.atomic[T] {

	override final def getJSON(json: JSONObject, key: String): T = deJSON(json.getJSONObject(key))

	override final def setJSON(json: JSONObject, key: String, value: T): Unit = json.put(key, enJSON(value))

	def enJSON(self: T): JSONObject

	def deJSON(json: JSONObject): T

	def >>>(f: T => T): member[T] = {
		val real: member[T] = this
		new member[T] {
			override def enJSON(self: T): JSONObject = real.enJSON(self)

			override def deJSON(json: JSONObject): T = f {
				real.deJSON(json)
			}

			override def decode(bytes: ByteBuffer): T =
				f {
					real.decode(bytes)
				}

			override def encode(self: T): ByteBuffer = real.encode(self)

			override def generate(random: Random): T =
				f {
					real.generate(random)
				}
		}
	}

	def ![O: ClassTag](get: O => T)(act: T => Either[O, member[O]]): member[O] = {
		val from: member[T] = this
		new member[O] {

			override def enJSON(self: O): JSONObject = {
				val act: Either[T => O, T => member[O]] = sys.error("switch what we take")
				val t: T = get(self)
				val sub = from.enJSON(t)
				act match {
					case Left(_) =>
						sub
					case Right(nn) =>
						sys.error("this is still packing more data into one JSON than I should ... right?")
						???
				}
			}

			override def deJSON(json: JSONObject): O =
				act(from.deJSON(json))
					.map((_: member[O]).deJSON(json))
					.merge

			override def decode(bytes: ByteBuffer): O =
				act(from.decode(bytes))
					.map((_: member[O]).decode(bytes))
					.merge

			override def encode(self: O): ByteBuffer = ???

			override def generate(random: Random): O =
				act(from.generate(random))
					.map(
						(_: member[O]).generate(random)
					)
					.merge
		}
	}
}


object member {
	val unByte = new atomic[Int] {

		implicit class UnsignedByteBuffer(data: ByteBuffer) {
			def getUnInt8: Int = (data.get() & 0xFF).toShort

			def putUnInt8(value: Int) = {
				require(0 <= value && value <= 255)

				data.put((value & 0xFF).toByte)
			}
		}

		override def decode(bytes: ByteBuffer): Int = bytes.getUnInt8

		override def encode(self: Int): ByteBuffer = ByteBuffer.allocate(4).putUnInt8(self)

		override def generate(random: Random): Int = random.nextInt(256).toShort

		override def getJSON(json: JSONObject, key: String): Int = {
			val out = json.getInt(key)
			require(0 <= out && out <= 255)
			out.toShort
		}

		override def setJSON(json: JSONObject, key: String, out: Int): Unit = {
			require(0 <= out && out <= 255)
			json.put(key, out)
		}
	}

	def seqS32[O, T: atomic](key: String, get: O => Seq[T]): TLink[O, List[T]] = {
		???
	}

	def listU8[O: ClassTag, V: ClassTag : atomic](key: String, get: O => Seq[V]): TLink[O, List[V]] = {
		streamU8[O, V](key, get).swap(_.toList, _.toStream)
	}

	def streamU8[O: ClassTag, V: ClassTag : atomic](key: String, get: O => Seq[V]): TLink[O, Stream[V]] = new TLink[O, Stream[V]] {

		class P(act: Stream[V] => Either[O, member[O]]) extends member[O] {
			override def enJSON(self: O): JSONObject = {
				val contents: Stream[V] = get(self).toStream

				act(contents) match {
					case Left(_) =>
						new JSONObject()
							.put(
								key,
								contents.zipWithIndex
									.foldLeft(new JSONArray()) {
										case (list, (value, index)) =>
											implicitly[atomic[V]].setJSON(list, index, value)
									}
							)

					case Right(what) =>
						what.enJSON(self)
							.put(
								key,
								contents.zipWithIndex
									.foldLeft(new JSONArray()) {
										case (list, (value, index)) =>
											implicitly[atomic[V]].setJSON(list, index, value)
									}
							)
				}
			}

			override def deJSON(json: JSONObject): O =
				act {
					val array: JSONArray =
						json.optJSONArray(key) match {
							case null => new JSONArray()
							case data => data
						}


					(0 until array.length())
						.toStream
						.map(implicitly[atomic[V]].getJSON(array, _: Int))
				} match {
					case Left(out) => out
					case Right(out) => out.deJSON(json)
				}

			override def decode(bytes: ByteBuffer): O =
				act {
					(0 until bytes.getUnsignedByte())
						.toStream
						.map {
							_ =>
								implicitly[atomic[V]].decode(bytes)
						}
						.force
				} match {
					case Left(out) => out
					case Right(out) => out.decode(bytes)
				}


			override def encode(self: O): ByteBuffer = {
				val list: Seq[V] = get(self)
				list
					.map(implicitly[atomic[V]].encode)
					.foldLeft(
						ByteBuffer.allocate(1)
							.putUnsignedByte(list.length)
					)((_: ByteBuffer) ++ (_: ByteBuffer))
			}

			override def generate(random: Random): O = {
				val len: Int = unByte.generate(random)
				act {
					Stream.continually {
						implicitly[atomic[V]].generate(random)
					}.take(len)
				}.map(_.generate(random))
					.merge
			}
		}

		class Q[E](left: atomic[E], act: E => Either[O, member[O]]) extends member[O] {
			override def enJSON(self: O): JSONObject = ???

			override def deJSON(json: JSONObject): O = ???

			override def decode(bytes: ByteBuffer): O = ???

			override def encode(self: O): ByteBuffer = ???

			override def generate(random: Random): O =
				act(left.generate(random))
					.map((_: member[O]).generate(random))
					.merge
		}

		override def map(act: Stream[V] => O): member[O] = new P((s: Stream[V]) => Left(act(s)))

		override def flatMap(act: Stream[V] => member[O]): member[O] = new P((s: Stream[V]) => Right(act(s)))
	}

	def of(domain: Iterable[Char]): atomic[Char] = new atomic[Char]() {
		private val list: List[Char] = domain.toList

		override def decode(bytes: ByteBuffer): Char = bytes.getChar()

		override def encode(self: Char): ByteBuffer = ByteBuffer.allocate(32).putChar(self)

		override def generate(random: Random): Char = list(random.nextInt(list.size))

		override def getJSON(json: JSONObject, key: String): Char =
			json.getString(key).toList match {
				case List(one) => one
				case _ =>
					sys.error("what?")
			}

		override def setJSON(json: JSONObject, key: String, value: Char): Unit =
			json.put(key, s"$value")
	}

	def apply[O: ClassTag, V: ClassTag : atomic](key: String, get: O => V): TLink[O, V] = new TLink[O, V] {
		val method: atomic[V] = implicitly

		override def map(act: V => O): member[O] = new member[O] {
			override def decode(bytes: ByteBuffer): O = act(method.decode(bytes))

			override def encode(self: O): ByteBuffer = {
				val them: V = get(self)
				method.encode(them)
			}

			override def generate(random: Random): O = act(method generate random)

			override def enJSON(self: O): JSONObject = {
				val json = new JSONObject()
				method.setJSON(json, key, get(self))
				json
			}

			override def deJSON(json: JSONObject): O =
				act {
					method.getJSON(json, key)
				}
		}

		override def flatMap(act: V => member[O]): member[O] = new member[O] {
			override def decode(bytes: ByteBuffer): O = act(method.decode(bytes)).decode(bytes)

			override def encode(self: O): ByteBuffer = {

				val them: V = get(self)
				val head: ByteBuffer = method.encode(them)
				val tail: ByteBuffer = act(them).encode(self)

				head ++ tail
			}

			override def generate(random: Random): O = act(method generate random).generate(random)

			override def enJSON(self: O): JSONObject = {

				val v: V = get(self)

				val json: JSONObject = act(v).enJSON(self)

				require(!json.has(key))

				method.setJSON(json, key, v)

				json
			}

			override def deJSON(json: JSONObject): O = {
				val v: V = method.getJSON(json, key)

				// TODO; purge keys?

				act(v).deJSON(json)
			}
		}
	}

	sealed trait atomic[T] {

		def decode(bytes: ByteBuffer): T

		def encode(self: T): ByteBuffer

		final def repeat(random: Random = new Random()): Stream[T] =
			Stream.continually(generate(random))

		def generate(random: Random = new Random()): T

		def getJSON(json: JSONObject, key: String): T

		def getJSON(json: JSONArray, key: Int): T =
			getJSON(new JSONObject().put(key.toString, json.get(key)), key.toString)


		def setJSON(json: JSONObject, key: String, value: T): Unit

		def setJSON(json: JSONArray, key: Int, value: T): JSONArray = {

			val side = new JSONObject()

			val keyString: String = key.toString

			setJSON(side, keyString, value)
			require(1 == side.keySet().size())

			json.put(
				key,
				side.get(keyString)
			)
		}
	}

	implicit def methodString(implicit memberChar: atomic[Char]): atomic[String] = {
		new atomic[String] {
			val from: member[String] =
				for {
					string <- member("string", (_: String).toCharArray, 8 to 260)
				} yield {
					new String(string.toArray)
				}

			override def decode(bytes: ByteBuffer): String = from.decode(bytes)

			override def encode(self: String): ByteBuffer = from.encode(self)

			override def generate(random: Random): String = from.generate(random)

			override def getJSON(json: JSONObject, key: String): String = json.getString(key)

			override def setJSON(json: JSONObject, key: String, value: String): Unit = json.put(key, value)
		}
	}

	implicit lazy val memberBoolean: atomic[Boolean] = new atomic[Boolean] {
		override def decode(bytes: ByteBuffer): Boolean =
			bytes.get() match {
				case 1 => true
				case 0 => false
				case what =>
					sys.error("what = " + what)
			}

		override def encode(self: Boolean): ByteBuffer =
			ByteBuffer.allocate(1)
				.put(
					(if (self)
						1
					else
						0).toByte
				)

		override def generate(random: Random): Boolean = random.nextBoolean()

		override def getJSON(json: JSONObject, key: String): Boolean = json.getBoolean(key)

		override def setJSON(json: JSONObject, key: String, value: Boolean): Unit = json.put(key, value)
	}
	implicit lazy val memberByte: atomic[Byte] = new atomic[Byte] {
		override def decode(bytes: ByteBuffer): Byte = bytes.get()

		override def encode(self: Byte): ByteBuffer = ByteBuffer.allocate(1).put(self)

		override def generate(random: Random): Byte = {
			val buf: Array[Byte] = Array[Byte](1)
			random.nextBytes(buf)
			buf.head
		}

		override def getJSON(json: JSONObject, key: String): Byte = json.getInt(key).toByte

		override def setJSON(json: JSONObject, key: String, value: Byte): Unit = json.put(key, value.toInt)
	}

	implicit lazy val memberInt: atomic[Int] = new atomic[Int] {
		override def decode(bytes: ByteBuffer): Int = bytes.getInt()

		override def encode(self: Int): ByteBuffer = ByteBuffer.allocate(32).putInt(self)

		override def generate(random: Random): Int = random.nextInt()

		override def getJSON(json: JSONObject, key: String): Int = json.getInt(key)

		override def setJSON(json: JSONObject, key: String, value: Int): Unit = json.put(key, value)
	}

	implicit lazy val memberFloat: atomic[Float] = new atomic[Float] {
		override def decode(bytes: ByteBuffer): Float = bytes.getFloat

		override def encode(self: Float): ByteBuffer = ByteBuffer.allocate(32).putFloat(self)

		override def generate(random: Random): Float =
			(random.nextFloat() * Float.MaxValue) + (Float.MinValue * random.nextFloat())

		override def getJSON(json: JSONObject, key: String): Float = json.getFloat(key)

		override def setJSON(json: JSONObject, key: String, value: Float): Unit = json.put(key, value)
	}

	sealed trait TLink[O, V] {
		def map(act: V => O): member[O]

		def flatMap(act: V => member[O]): member[O]


		final def foreach(act: V => member[O]): member[O] = flatMap(act)

		def swap[Q](i: V => Q, f: Q => V): TLink[O, Q] = {
			val base = this
			new TLink[O, Q] {
				override def map(act: Q => O): member[O] =
					base.map(i andThen act)

				override def flatMap(act: Q => member[O]): member[O] =
					base.flatMap(i andThen act)
			}
		}
	}

	object charVisible {
		val alphas: immutable.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z')
		val numeric: immutable.IndexedSeq[Char] = ('0' to '9') ++ "+-."
		val more = "`!\"£$%^&*()_+-=\\|{}[]:@~;'#,./<>?"

		def apply(): atomic[Char] = of(alphas ++ numeric ++ more ++ " ")
	}

	/**
	 *
	 * @param key    they name for this member when encoded to JSON
	 * @param get    a lambad to grab the values
	 * @param counts numbers of things to use when generating arrays
	 * @param method the interface for the field type
	 * @tparam O the object type
	 * @tparam V the field type
	 * @return a thing to build a member
	 */
	//	@deprecated
	def apply[O: ClassTag, V: ClassTag](key: String, get: O => Iterable[V], counts: Iterable[Int])(implicit method: atomic[V]): TLink[O, Stream[V]] = new TLink[O, Stream[V]] {
		override def map(act: Stream[V] => O): member[O] =
			new member[O] {
				override def decode(bytes: ByteBuffer): O =
					act {
						bytes.get32(method.decode).toStream
					}

				override def encode(self: O): ByteBuffer = {
					val vs: Stream[V] = get(self).toStream
					byteBuffer32(vs)(method.encode)
				}

				override def generate(random: Random): O =
					act {
						(0 until random.next(counts))
							.map {
								_: Int =>
									method.generate(random)
							}.toStream.force
					}

				override def enJSON(self: O): JSONObject =
					new JSONObject()
						// TODO; generalise this put
						.put(key, get(self)
							.foldLeft(new JSONArray()) {
								case (list, next: V) =>

									val temp = new JSONObject()

									method.setJSON(temp, key, next)

									list.put(temp.get(key))
							}
						)


				override def deJSON(json: JSONObject): O = {
					// TODO; generalise this get
					act {
						val list: JSONArray = json.getJSONArray(key)
						(0 until list.length())
							.toStream
							.map {
								i: Int =>
									method
										.getJSON(
											new JSONObject()
												.put(key, list.get(i)),
											key
										)
							}
					}
				}
			}

		override def flatMap(act: Stream[V] => member[O]): member[O] =
			new member[O] {
				override def decode(bytes: ByteBuffer): O =
					act {
						bytes.get32(method.decode)
							.toStream
					} decode bytes

				override def encode(self: O): ByteBuffer = {
					val vs: Stream[V] = get(self).toStream
					byteBuffer32(vs)(method.encode) ++ act(vs).encode(self)
				}

				override def generate(random: Random): O = {
					act {
						(0 until random.next(counts))
							.map {
								_: Int =>
									method.generate(random)
							}.toStream.force
					} generate random
				}

				override def enJSON(self: O): JSONObject = {


					val vs: Iterable[V] = get(self)

					val json: JSONObject = act(vs.toStream).enJSON(self)
					json
						// TODO; generalise this put
						.put(key, vs
							.foldLeft(new JSONArray()) {
								case (list, next: V) =>

									val temp = new JSONObject()

									method.setJSON(temp, key, next)

									list.put(temp.get(key))
							}
						)
				}

				override def deJSON(json: JSONObject): O =
				// TODO; generalise this get
					act {
						val list: JSONArray = json.getJSONArray(key)
						(0 until list.length())
							.toStream
							.map {
								i: Int =>
									method
										.getJSON(
											new JSONObject()
												.put(key, list.get(i)),
											key
										)
							}
					} deJSON json
			}
	}


}
