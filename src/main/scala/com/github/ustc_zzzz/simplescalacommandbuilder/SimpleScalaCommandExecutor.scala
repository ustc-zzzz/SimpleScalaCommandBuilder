package com.github.ustc_zzzz.simplescalacommandbuilder

import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandImplicits._
import org.spongepowered.api.command.args.{CommandContext, CommandElement, CommandFlags, GenericArguments => global}
import org.spongepowered.api.command.spec.{CommandExecutor, CommandSpec}
import org.spongepowered.api.command.{CommandException, CommandResult, CommandSource}
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.text.Text

import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * @author ustc_zzzz
  */
class SimpleScalaCommandExecutor[+T](arg: SimpleScalaCommandExecutor.Arg[T], executor: T => Try[_]) {
  def addTo(builder: CommandSpec.Builder): Unit = builder.arguments(arg.toSponge).executor(arg.toExecutor(executor))
}

object SimpleScalaCommandExecutor {

  sealed trait OrSource[+T] extends ListArg[T] {
    def or(u: Source): ListArg[T]
  }

  sealed trait WithFlag[+T] extends SingleArg[T] { t =>
    def withBuilder(b: CommandFlags.Builder): CommandElement

    def toSponge: CommandElement = withBuilder(global.flags())

    override def and[V, U](u: FlagArg[U])(implicit st: SeqTransformer[V, T, U]): WithFlag[V] = new WithFlag[V] {
      override def withBuilder(b: CommandFlags.Builder): CommandElement = t.withBuilder(u.withBuilder(b))

      override def toArg(src: CommandSource, args: CommandContext): Try[V] = st(t.toArg(src, args), u.toArg(src, args))
    }
  }

  sealed trait Arg[+T] { t =>
    def toSponge: CommandElement

    def toArg(src: CommandSource, args: CommandContext): Try[T]

    def toExecutor(executor: T => Try[Any]): CommandExecutor = (src, args) => toArg(src, args).flatMap(executor) match {
      case Success(result: CommandResult) => result
      case Success(_) => CommandResult.success()
      case Failure(e) => throw e
    }

    def and[V, U](u: Arg[U])(implicit st: SeqTransformer[V, T, U]): SingleArg[V] = new SingleArg[V] {
      private val none: CommandElement = global.none()

      override def toSponge: CommandElement = (t.toSponge, u.toSponge) match {
        case (element1, this.none) => element1
        case (this.none, element2) => element2
        case (element1, element2) => global.seq(element1, element2)
      }

      override def toArg(src: CommandSource, args: CommandContext): Try[V] = st(t.toArg(src, args), u.toArg(src, args))
    }

    def and[V, U](u: FlagArg[U])(implicit st: SeqTransformer[V, T, U]): WithFlag[V] = new WithFlag[V] {
      override def withBuilder(arg: CommandFlags.Builder): CommandElement = u.withBuilder(arg).buildWith(t.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[V] = st(t.toArg(src, args), u.toArg(src, args))
    }

    def opt: SingleArg[Option[T]] = new SingleArg[Option[T]] {
      override def toSponge: CommandElement = global.optionalWeak(t.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[Option[T]] = {
        Success[Option[T]](t.toArg(src, args).toOption)
      }
    }
  }

  sealed trait FlagArg[+T] extends Arg[T] { t =>
    def withBuilder(b: CommandFlags.Builder): CommandFlags.Builder

    def toSponge: CommandElement = withBuilder(global.flags()).buildWith(global.none())

    def only[U](implicit ev: T <:< List[U]): FlagArg[Option[U]] = new FlagArg[Option[U]] {
      override def withBuilder(b: CommandFlags.Builder): CommandFlags.Builder = t.withBuilder(b)

      override def toArg(src: CommandSource, args: CommandContext): Try[Option[U]] = t.toArg(src, args) match {
        case Success(list) if list.size > 1 => Try(None)
        case Success(list) => Try(list.headOption)
        case Failure(e) => Failure(e)
      }
    }

    def required[U](implicit ev: T <:< Option[U]): FlagArg[U] = new FlagArg[U] {
      override def withBuilder(b: CommandFlags.Builder): CommandFlags.Builder = t.withBuilder(b)

      override def toArg(src: CommandSource, args: CommandContext): Try[U] = t.toArg(src, args).flatMap(u => Try {
        u.getOrElse(throw new CommandException(Text.of(s"No value present for ${t.toSponge.getUntranslatedKey}")))
      })
    }
  }

  sealed trait SingleArg[+T] extends Arg[T] { t =>
    def toArgs(src: CommandSource, args: CommandContext): Iterable[T] = toArg(src, args).map(List(_)).getOrElse(Nil)

    def or[U >: T](u: SingleArg[U]): SingleArg[U] = new SingleArg[U] {
      override def toSponge: CommandElement = global.firstParsing(t.toSponge, u.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[U] = {
        t.toArg(src, args) orElse u.toArg(src, args)
      }
    }

    def rep(times: Int): ListArg[T] = new ListArg[T] {
      override def toSponge: CommandElement = global.repeated(t.toSponge, times)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = {
        Try[List[T]](t.toArgs(src, args).toList)
      }
    }

    def all: ListArg[T] = new ListArg[T] {
      override def toSponge: CommandElement = global.allOf(t.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = {
        Try[List[T]](t.toArgs(src, args).toList)
      }
    }

    def required[U](implicit ev: T <:< Option[U]): SingleArg[U] = new SingleArg[U] {
      override def toSponge: CommandElement = t.toSponge

      override def toArg(src: CommandSource, args: CommandContext): Try[U] = t.toArg(src, args).flatMap(u => Try {
        u.getOrElse(throw new CommandException(Text.of(s"No value present for ${t.toSponge.getUntranslatedKey}")))
      })
    }
  }

  sealed trait ListArg[+T] extends Arg[List[T]] { t =>
    def or[U >: T](u: ListArg[U]): ListArg[U] = new ListArg[U] {
      override def toSponge: CommandElement = global.firstParsing(t.toSponge, u.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[U]] = {
        t.toArg(src, args) orElse u.toArg(src, args)
      }
    }

    def only: SingleArg[Option[T]] = new SingleArg[Option[T]] {
      override def toSponge: CommandElement = global.onlyOne(t.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[Option[T]] = t.toArg(src, args) match {
        case Success(list) if list.size > 1 => Try(None)
        case Success(list) => Try(list.headOption)
        case Failure(e) => Failure(e)
      }

      override def toArgs(src: CommandSource, args: CommandContext): Iterable[Option[T]] = {
        t.toArg(src, args).getOrElse(Nil).map(Some(_))
      }
    }

    def rep(times: Int): ListArg[T] = new ListArg[T] {
      override def toSponge: CommandElement = global.repeated(t.toSponge, times)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = t.toArg(src, args)
    }

    def all: ListArg[T] = new ListArg[T] {
      override def toSponge: CommandElement = global.allOf(t.toSponge)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = t.toArg(src, args)
    }
  }

  class Source extends SingleArg[CommandSource] {
    def toSponge: CommandElement = global.none()

    def toArg(src: CommandSource, args: CommandContext): Try[CommandSource] = Try[CommandSource](src)
  }

  class Flag(private val ids: Seq[String]) extends FlagArg[List[Unit]] {

    def withBuilder(b: CommandFlags.Builder): CommandFlags.Builder = b.flag(ids: _*)

    def toArg(src: CommandSource, args: CommandContext): Try[List[Unit]] = Try {
      List.fill(ids.map(args.getAll(_).size).sum)(())
    }

    def or(u: Flag): Flag = new Flag(ids ++ u.ids)

    def of[U](parameter: ListArg[U]): FlagArg[List[U]] = new FlagArg[List[U]] {
      def withBuilder(b: CommandFlags.Builder): CommandFlags.Builder = b.valueFlag(parameter.toSponge, ids: _*)

      def toArg(src: CommandSource, args: CommandContext): Try[List[U]] = parameter.toArg(src, args)
    }
  }

  class GetOne[+T](id: String, getter: Text => CommandElement) extends SingleArg[T] {

    override def toSponge: CommandElement = getter(Text.of(id))

    override def toArg(src: CommandSource, args: CommandContext): Try[T] = Try[T](args.getOne[T](id).get)

    override def toArgs(src: CommandSource, args: CommandContext): Iterable[T] = args.getAll[T](id).asScala
  }

  class GetList[+T](id: String, getter: Text => CommandElement) extends ListArg[T] {

    import scala.collection.JavaConverters._

    override def toSponge: CommandElement = getter(Text.of(id))

    override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = {
      Try[List[T]](args.getAll[T](id).asScala.toList)
    }
  }

  class GetUser(id: String) extends GetList[User](id, global.user) with OrSource[User] { t =>
    override def or(u: Source): ListArg[User] = new ListArg[User] {
      override def toSponge: CommandElement = global.userOrSource(t.toSponge.getKey)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[User]] = t.toArg(src, args)
    }
  }

  class GetPlayer(id: String) extends GetList[Player](id, global.player) with OrSource[Player] { t =>
    override def or(u: Source): ListArg[Player] = new ListArg[Player] {
      override def toSponge: CommandElement = global.playerOrSource(t.toSponge.getKey)

      override def toArg(src: CommandSource, args: CommandContext): Try[List[Player]] = t.toArg(src, args)
    }
  }

  class FromSingle[+T](arg: SingleArg[T]) extends ListArg[T] {
    override def toSponge: CommandElement = arg.toSponge

    override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = {
      Try[List[T]](arg.toArgs(src, args).toList)
    }
  }

  class FromOption[+T](arg: Arg[Option[T]]) extends ListArg[T] {
    override def toSponge: CommandElement = arg.toSponge

    override def toArg(src: CommandSource, args: CommandContext): Try[List[T]] = arg.toArg(src, args) match {
      case Success(Some(t)) => Success(List(t))
      case Success(None) => Success(Nil)
      case Failure(e) => Failure(e)
    }
  }
}
