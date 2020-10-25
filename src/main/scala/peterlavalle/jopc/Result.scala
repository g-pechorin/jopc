package peterlavalle.jopc


object Result {
	type Result[T] = Either[Exception, T]

	def Failure(message: String): Result[Nothing] = Left(new Exception(message))

	def Success[T](value: T): Result[T] = Right(value)

	//	case class ExtraFields private(names: Set[String]) extends Failure {
	//		override def message: String = "there were unused fields " + names.toList.sorted.toString().drop(4)
	//	}

	object Failure {
		def unapply(arg: Result[_]): Option[String] =
			arg match {
				case Right(_) =>
					None
				case Left(failure) =>
					Some(failure.getMessage)
			}
	}

}
