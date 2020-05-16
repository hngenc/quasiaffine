package quasiaffine

final case class QuasiAffineException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
