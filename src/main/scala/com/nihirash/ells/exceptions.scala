package com.nihirash.ells

final case class EllsTypesException(message: String = "Non expected type received", cause: Throwable = None.orNull)
    extends RuntimeException(message, cause)

final case class EllsArityException(message: String = "Wrong number of args passed", cause: Throwable = None.orNull)
    extends RuntimeException(message, cause)

final case class EllsEvalException(message: String = "Can't eval form", cause: Throwable = None.orNull)
    extends RuntimeException(message, cause)

final case class EllsDefinitionNotFound(message: String = "Trying to eval undefined value",
                                        cause: Throwable = None.orNull)
    extends RuntimeException(message, cause)

final case class EllsRuntimeException(message: String = "Runtime error") extends RuntimeException(message, None.orNull)

final case class EllsInternalError(message: String = "Internal Error", cause: Throwable = None.orNull)
    extends RuntimeException(message, cause)
