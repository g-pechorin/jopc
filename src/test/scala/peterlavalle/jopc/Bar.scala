package peterlavalle.jopc

case class Bar(s: Stream[String], f: Set[Foo])

object Bar {


	implicit def methodBar: member[Bar] = {

		implicit val chars: member.atomic[Char] =
			member.of((('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')))

		for {
			f <- member("f", (_: Bar).f, 3 to 14)
			s <- member("s", (_: Bar).s, 8 to 30)
		} yield {
			Bar(
				s, f.toSet
			)
		}
	}
}
