package peterlavalle.jopc

import org.json.JSONObject
import org.scalatest.funsuite.AnyFunSuite
import peterlavalle.jopc.member.memberInt

class MemberTests extends AnyFunSuite {

	def loopInAndOut[T: member.atomic](n: Int = 40000): Unit = {
		val method: member.atomic[T] = implicitly[member.atomic[T]]
		method.repeat().take(n)
			.map {
				expected: T =>
					assert(
						method.decode {
							method
								.encode(expected)
								.flip()
						} == expected
					)

					method match {
						case method: member[T] =>
							val json: JSONObject = method.enJSON(expected)
							val jsonActual: T = method.deJSON(json)
							assert(jsonActual == expected)
						case _ =>

					}
			}
	}

	trait Base {
		var x: Int = 0
	}

	case class Foo(f: Float) extends Base

	case class Bar(i: Int) extends Base

	test("do some floats") {
		loopInAndOut[Float](128)
	}

	test("seq") {
		loopInAndOut(128) {
			case class Chain(b: Boolean, i: List[Int])
			for {
				b <- member("b", (_: Chain).b)
				i <- member.listU8("i", (_: Chain).i)
			} yield {
				Chain(b, i)
			}
		}
	}

	test("test Foo (canary)") {
		loopInAndOut() {
			for {
				x <- member("x", (_: Foo).x)
				f <- member("f", (_: Foo).f)
			} yield {
				val foo: Foo = Foo(f)
				foo.x = x
				foo
			}
		}
	}

	test("test something with a bool") {

		case class Flop(b: Boolean)

		loopInAndOut() {
			for {
				b <- member("b", (_: Flop).b)
			} yield {
				Flop(b)
			}
		}
	}

	test("test something with just a sint32") {
		case class Flop(b: Int)
		loopInAndOut() {
			for {
				b <- member("b", (_: Flop).b)
			} yield {
				Flop(b)
			}
		}
	}

	test("test something with just a float32") {
		case class Flop(b: Float)
		loopInAndOut() {
			for {
				b <- member("b", (_: Flop).b)
			} yield {
				Flop(b)
			}
		}
	}

	test("text foreach/extended") {

		def select: String => member[Base] = {
			case "foo" =>
				for {
					f <- member("f", (_: Base).asInstanceOf[Foo].f)
				} yield {
					Foo(f)
				}
			case "bar" =>
				for {
					f <- member("i", (_: Base).asInstanceOf[Bar].i)
				} yield {
					Bar(f)
				}
		}

		loopInAndOut() {
			for {
				x <- member("x", (_: Base).x)
				c <- member("c", (_: Base).isInstanceOf[Foo])
			} {
				select(if (c) "foo" else "bar") >>> {
					v: Base =>
						v.x = x
						v
				}
			}
		}
	}
}
