package peterlavalle.jopc

import java.nio.ByteBuffer
import java.util.Random

import org.json.{JSONArray, JSONObject}

import scala.annotation.tailrec

object include extends include

trait include extends Coder {

	implicit class ExtRandom(random: Random) {
		def next[O](items: Iterable[O]): O =
			items.toList.apply(random.nextInt(items.size))
	}

	def byteBuffer32[O](d: Iterable[O])(f: O => ByteBuffer): ByteBuffer =
		ByteBuffer.allocate(12).put32(d)(f)

	implicit class BytesBuffer(self: ByteBuffer) {
		require(0 == self.arrayOffset())

		def get32[O](f: ByteBuffer => O): List[O] =
			(0 until self.getInt)
				.map {
					_ =>
						f(self)
				}
				.toList

		def put32[O](d: Iterable[O])(f: O => ByteBuffer): ByteBuffer = {
			d.foldLeft(self.putInt(d.size))(_ ++ f(_))
		}

		def ++(last: ByteBuffer): ByteBuffer = {
			if (self.remaining() > last.position()) {
				(last.arrayOffset() until last.position())
					.map(last.array())
					.foldLeft(self)((_: ByteBuffer) put (_: Byte))
			} else {
				val oldData: Seq[Byte] =
					(0 until (self.position() + last.position()))
						.map {
							i: Int =>
								if (i < self.position())
									self.array()(i)
								else
									last.array()(i - self.position())
						}

				val extraSpace: Array[Byte] = Array.ofDim[Byte](32)

				ByteBuffer
					.wrap(
						(oldData ++ extraSpace).toArray
					)
					.position(oldData.length)
			}
		}

		def putUnsignedByte(value: Int): ByteBuffer = {
			require(0 <= value && value <= 255)
			self.put(
				(value & 0xff).toByte
			)
		}

		def getUnsignedByte(): Int = {
			val value: Int = self.get() & 0xFF
			require(0 <= value && value <= 255)
			value
		}
	}


	private final def deepEq(o: => (JSONObject, JSONObject), a: => (JSONArray, JSONArray), v: => (String, String)): Boolean = (o) match {
		case (self: JSONObject, them: JSONObject) =>
			self deepEqu them
		case (null, null) =>
			(a) match {
				case (self: JSONArray, them: JSONArray) =>
					self deepEqu them
				case (null, null) =>
					val (l, r) = v
					l == r
			}
	}

	implicit class PrimpJSONArray(self: JSONArray) {

		def toStreamOf[T](map: (JSONArray, Int) => T): Stream[T] = {
			(0 until self.length())
				.toStream
				.map(map(self, _: Int))
		}

		def clear(): Unit =
			while (0 != self.length())
				self.remove(0)


		def deepEqu(them: JSONArray): Boolean =
			(0 until self.length()).foldLeft(self.length() == them.length()) {
				case (false, _) => false
				case (_, next) =>
					deepEq(
						(self.optJSONObject(next), them.optJSONObject(next)),
						(self.optJSONArray(next), them.optJSONArray(next)),
						(self.get(next).toString, them.get(next).toString)
					)
			}
	}

	implicit class PrimpJSONObject(self: JSONObject) {

		def deepEqu(them: JSONObject): Boolean = {
			val selfKeys: List[String] = self.toListOfKeys
			val themKeys: List[String] = them.toListOfKeys
			selfKeys.foldLeft(selfKeys == themKeys) {
				case (false, _) => false
				case (_, next) =>
					deepEq(
						(self.optJSONObject(next), them.optJSONObject(next)),
						(self.optJSONArray(next), them.optJSONArray(next)),
						(self.get(next).toString, them.get(next).toString)
					)
			}
		}

		def toListOfKeys: List[String] = toSetOfKeys.toList.sorted

		def toSetOfKeys: Set[String] = self.keySet().toArray.toList.map((_: AnyRef).asInstanceOf[String]).toSet

		def set(key: String): set =
			new set {
				override def ?(s: String): Boolean = load(s)

				@tailrec
				private def data: JSONArray = {
					val data: JSONArray = self.optJSONArray(key)
					if (null != data)
						data
					else {
						self.put(key, new JSONArray())
						this.data
					}
				}

				private def load: Set[String] =
					data
						.toStreamOf((_: JSONArray) getString (_: Int))
						.toSet

				private def save(v: Iterable[String]): Unit = {
					val full: List[String] =
						v
							.toList
							.sorted

					while (data.length() > full.size)
						data.remove(0)

					full
						.zipWithIndex
						.foreach {
							case (value, index) =>
								data.put(index, value)
						}

				}

				override def +=(s: String): Unit = save(load + s)

				override def -=(s: String): Unit = save(load filterNot ((_: String) == s))

				override def iterator: Iterator[String] = load.toList.sorted.iterator
			}

		def /(path: String): JSONObject =
			if (path.isEmpty)
				self
			else {
				val head: String = path.takeWhile('/' != (_: Char))

				self.extJSONObject(head, new JSONObject()) / path.dropWhile('/' != (_: Char)).drop(1)
			}

		def extJSONArray(key: String, value: => JSONArray = new JSONArray()): JSONArray = {

			val out: JSONArray = self.optJSONArray(key)

			if (null != out)
				out
			else {
				self.put(key, value: JSONArray)
					.get(key)
					.asInstanceOf[JSONArray]
			}
		}

		def extJSONObject(key: String, value: => JSONObject = new JSONObject()): JSONObject = {

			val out: JSONObject = self.optJSONObject(key)

			if (null != out)
				out
			else {
				self.put(key, value: JSONObject)
					.get(key)
					.asInstanceOf[JSONObject]
			}
		}

		trait set extends Iterable[String] {
			@deprecated
			final def apply(s: String): Boolean = this ? s

			def ?(s: String): Boolean

			def +=(s: String): Unit

			def -=(s: String): Unit
		}

	}


}
