package peterlavalle.jopc

import java.nio.ByteBuffer

import org.scalatest.funsuite.AnyFunSuite

class CodeAsTest extends AnyFunSuite {

	import Bar._
	import Foo._

	test("foo in") {
		(0 until 40000).foreach {
			_: Int =>
				methodFoo.encode(methodFoo.generate())
		}
	}

	test("foo 40 in then out") {

		// generate
		val src =
			(0 until 40)
				.map(_ => methodFoo.generate())

		// encode
		val dat =
			src.map(methodFoo.encode).reduce(_ ++ _)

		// flip the buffer to be read-from
		dat.flip()

		// decode and test
		src.foreach {
			expected: Foo =>
				assert(methodFoo.decode(dat) == expected)
		}
	}


	test("bar 84 in then out") {

		// generate
		val src: Seq[Bar] =
			(0 until 84)
				.map(_ => methodBar.generate())

		// encode
		val dat: ByteBuffer =
		//		src.map(codeOfBar.encode).foldLeft(Stream[Byte]())(_ ++ _)
			src.map(methodBar.encode).reduce(_ ++ _)

		// flip the buffer to be read-from
		dat.flip()

		// decode and test
		src.foreach {
			expected: Bar =>
				assert(methodBar.decode(dat) == expected)
		}
	}
}
