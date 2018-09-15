package com.nihirash.ells

final case class EllsTypesException(message: String = "Non expected type received", cause: Throwable = None.orNull) extends RuntimeException(message, cause)

final case class EllsArityException(message: String = "Wrong number of args passed", cause: Throwable = None.orNull) extends RuntimeException(message, cause)

final case class EllsEvalException(message: String = "Can't eval form", cause: Throwable = None.orNull) extends RuntimeException(message, cause)