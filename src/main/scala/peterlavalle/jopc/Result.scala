package peterlavalle.jopc


object Result {

	type Result[T] = Either[Exception, T]

	def apply[T](value: T): Result[T] = Right(value)

	def unapply[T](arg: Result[T]): Option[T] =
		arg match {
			case Right(value) =>
				Some(value)
			case Left(_) =>
				None
		}

	implicit def implicitFailure(exception: FailureException): Result[Nothing] = Left(exception)

	implicit class PiResult[T](result: Result[T]) {
		def value: T =
			result match {
				case Result(value) =>
					value
				case Failure(exception: Exception) =>
					throw exception
			}

		def orElse[E >: T, Q <: E](otherwise: => Q): Result[E] =
			if (result.isRight)
				result
			else
				Result(otherwise)
	}

	trait FailureException extends java.lang.Exception

	case class UnstableAtomicValue
	(
		atomic: String,
		value: String,
	) extends Exception(
		s"unstable conversion :$atomic = `$value`"
	) with FailureException

	object Failure {
		def apply(message: String): Result[Nothing] = Left(new Exception(message))

		def unapply(arg: Result[_]): Option[Exception] =
			arg match {
				case Right(_) =>
					None
				case Left(failure) =>
					Some(failure)
			}
	}

}
