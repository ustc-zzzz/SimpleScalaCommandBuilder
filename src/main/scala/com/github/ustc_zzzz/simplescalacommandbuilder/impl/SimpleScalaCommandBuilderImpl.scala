package com.github.ustc_zzzz.simplescalacommandbuilder.impl

import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandBuilder
import com.google.inject.Inject
import org.slf4j.Logger
import org.spongepowered.api.event.game.state.GameStartingServerEvent
import org.spongepowered.api.event.{Listener, Order}
import org.spongepowered.api.plugin.Plugin
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.format.TextColors

import scala.collection.mutable

/**
  * @author ustc_zzzz
  */
// noinspection SpellCheckingInspection
@Plugin(
  version = "0.1.0",
  id = "simplescalacommandbuilder",
  name = "SimpleScalaCommandBuilder",
  authors = Array("ustc_zzzz"),
) class SimpleScalaCommandBuilderImpl {
  // noinspection VarCouldBeVal
  @Inject private[this] var logger: Logger = _

  @Listener(order = Order.LAST) def onStartingServer(event: GameStartingServerEvent): Unit = {
    for ((plugin, builder) <- SimpleScalaCommandBuilderImpl.tasks) builder().register(plugin)
    logger.info(s"Registered ${SimpleScalaCommandBuilderImpl.tasks.size} command(s).")
  }

  import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandBuilder._

  this.registry += command {
    name("simplescalacommandtest", "ssctest", "ssct")
    permission("simplescalacommandbuilder.command.test")
    description("Test Command for SimpleScalaCommandBuilder.")
    command {
      name("test-send", "send", "ts", "s")
      executor("test-string".str and "test-number".int and ("test-sender".player or src).only.required) {
        case (str, int, player) =>
          player.sendMessage(Text.of(TextColors.GREEN, f"$str with $int"))
      }
    }
    command {
      name("gu")
      executor(src and ("gu".literal rep 5) and "world".world) {
        case (src, _, worlds) =>
          for (world <- worlds; name = world.getWorldName) src.sendMessage(Text.of(f"gu gu gu gu gu gu in $name"))
      }
    }
    command {
      name("sum")
      executor(src and "number".int.all and ("-f".flag or "--format".flag of "format-string".str).only) {
        case (src, Nil, _) =>
          src.sendMessage(Text.of(TextColors.GREEN, "nothing found, and it is 0."))
        case (src, ints, Some(format)) =>
          src.sendMessage(Text.of(TextColors.GREEN, format.format(ints.mkString(" + "), ints.sum.toString)))
        case (src, ints, None) =>
          src.sendMessage(Text.of(TextColors.GREEN, "%s is %s".format(ints.mkString(" + "), ints.sum.toString)))
      }
    }
  }
}

object SimpleScalaCommandBuilderImpl {
  private type Task = (Object, () => SimpleScalaCommandBuilder)
  private[simplescalacommandbuilder] val tasks: mutable.MutableList[Task] = mutable.MutableList()
}
