package peterlavalle.jopc

import java.util.Random

import org.scalatest.funsuite.AnyFunSuite

class RandOfTest extends AnyFunSuite {

	import Bar._
	import Foo._


	test("just a foo") {
		val orwell: Foo = methodFoo.generate(new Random(1984))
		assert(Foo(-1623339239, 1.9331278E38f) == orwell)
	}

	test("40 bars") {
		(0 until 40).foreach {
			_: Int =>
				assert(null != methodBar.generate())
		}
	}
}
