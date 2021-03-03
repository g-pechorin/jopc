package peterlavalle.jopc

import org.scalatest.funsuite.AnyFunSuite

class AlternativeTest extends AnyFunSuite {

	test("compute from alternatives ") {
		trait A

		case class B(i: Int) extends A

		case class C(f: Float) extends A


		val mB: member[B] =
			for {
				i <- member("i", (_: B).i)
			} yield {
				B(i)
			}
		val mC =
			for {
				f <- member("f", (_: C).f)
			} yield {
				C(f)
			}


		implicit val char = member.charVisible()

		val subject: member[A] =
			for {
				c <- member("c", (_: A).getClass.getSimpleName.takeWhile('$' != (_: Char)))
			} {
				c match {
					case "C" =>
						mC.asInstanceOf[member[A]]
					case "B" =>
						mB.asInstanceOf[member[A]]
				}
			}

		val bIn: B = B(23)
		val b23 =
			subject.enJSON(bIn)

		val bOut = subject.deJSON(b23)

		assert(bIn == bOut)
	}
}
