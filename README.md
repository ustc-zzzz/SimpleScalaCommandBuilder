# SimpleScalaCommandBuilder

Type safe, simple, intuitive Sponge command builder for Scala.

Example (in the plugin main class annotated with `@Plugin`):

```scala
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
```

