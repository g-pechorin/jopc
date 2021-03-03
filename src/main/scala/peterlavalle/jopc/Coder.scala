package peterlavalle.jopc

import org.json.{JSONException, JSONObject}

import scala.reflect.{ClassTag, classTag}

object Coder extends Coder {

	implicit def deMember[T](implicit method: member[T]): De[T] =
		(json: JSONObject) =>
			try {
				Some(method.deJSON(json))
			} catch {
				case e: JSONException =>
					???
				case e =>

					sys.error("wrap/handle")

					???

					throw e
			}

	/**
	 * implement this and make it implicitly visible to allow decoding of things from JSON objects
	 *
	 */
	trait De[T] {
		def decode(json: JSONObject): Option[T]

		def map[R](f: T => R): De[R] = decode(_: JSONObject).map(f)

		def |[Q >: T, E <: Q](next: De[E]): De[Q] = {
			implicit val t: De[T] = this
			implicit val e: De[E] = next
			(json: JSONObject) =>
				json
					.decode[T]
					.map((_: T).asInstanceOf[Q])
					.orElse {
						json.decode[E]
					}
		}
	}

	implicit def enMember[T](implicit method: member[T]): En[T] =
		(data: T) =>
			method.enJSON(data)

	/**
	 * implement this and make it implicitly visible to allow encoding of things to JSON objects
	 *
	 */
	trait En[T] {
		def encode(data: T): JSONObject

		def map[L](f: L => T): En[L] = (l: L) => encode(f(l))

		def |[Q >: T, E <: Q : ClassTag](next: En[E]): En[Q] = {
			implicit val t: En[T] = this
			implicit val e: En[E] = next
			(q: Q) =>
				if (classTag[E].runtimeClass.isInstance(q))
					e.encode(q.asInstanceOf[E])
				else
					t.encode(q.asInstanceOf[T])
		}
	}

}

trait Coder {

	import Coder._

	implicit class EncodeAnyRef[C: En](data: C) {
		def encode: JSONObject = implicitly[En[C]].encode(data)
	}

	implicit class DecodeJSONObject(json: JSONObject) {
		def decode[C: De]: Option[C] = implicitly[De[C]].decode(json)
	}

}
