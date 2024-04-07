package org.alexn.hook

import cats.effect.IOApp

object Main extends IOApp.Simple:
   val run = HookServer.run
