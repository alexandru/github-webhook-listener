package org.alexn.hook

/**
 * Exception thrown when there is a configuration error,
 * see [AppConfig].
 */
class ConfigException(
    message: String,
    cause: Throwable? = null
) : Exception(message, cause)