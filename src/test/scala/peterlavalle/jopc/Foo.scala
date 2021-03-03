package peterlavalle.jopc

case class Foo(i: Int, f: Float)

object Foo {

	import member._

	implicit val methodFoo: member[Foo] =
		for {
			i <- member("i", (_: Foo).i)
			f <- member("f", (_: Foo).f)
		} yield {
			Foo(i, f)
		}
}
