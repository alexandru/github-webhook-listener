package org.alexn.hook

import io.circe.Codec
import scala.deriving.Mirror

extension (self: Codec.type)
    inline def derived[A](using inline A: Mirror.Of[A]) =
        io.circe.generic.semiauto.deriveCodec
