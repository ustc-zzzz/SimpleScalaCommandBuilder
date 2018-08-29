package com.github.ustc_zzzz.simplescalacommandbuilder

import com.flowpowered.math.vector.Vector3d
import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandExecutor._
import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandImplicits._
import org.spongepowered.api.command.CommandCallable
import org.spongepowered.api.command.args.{GenericArguments => global}
import org.spongepowered.api.command.spec.CommandSpec
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.storage.WorldProperties
import org.spongepowered.api.world.{Dimension, Location, World}
import org.spongepowered.api.{CatalogType, Sponge}

import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Try}

/**
  * @author ustc_zzzz
  */
class SimpleScalaCommandBuilder {

  def register(plugin: AnyRef): Unit = {
    if (aliases.isEmpty) throw new IllegalStateException("Name required")
    Sponge.getCommandManager.register(plugin, this (), aliases: _*)
  }

  private def apply(): CommandCallable = {
    val builder = CommandSpec.builder()
    for (executor <- executor) executor.addTo(builder)
    for (desc <- description) builder.description(desc)
    for (permission <- permission) builder.permission(permission)
    for (child <- children) builder.child(child(), child.aliases: _*)
    builder.build()
  }

  private var aliases: Seq[String] = Nil
  private var description: Option[Text] = None
  private var permission: Option[String] = None
  private var children: List[SimpleScalaCommandBuilder] = Nil
  private var executor: Option[SimpleScalaCommandExecutor[_]] = None
}

object SimpleScalaCommandBuilder extends TransformImplicits with SeqTransformImplicits {

  def name(names: String*): Unit = context.value.aliases = names

  def description(desc: Text): Unit = context.value.description = Some(desc)

  def description(desc: String): Unit = context.value.description = Some(Text.of(desc))

  def permission(permission: String): Unit = context.value.permission = Some(permission)

  def executor[T](arg: Arg[T])(f: T => Any): Unit = {
    context.value.executor = Some(new SimpleScalaCommandExecutor(arg, (t: T) => Try(f(t))))
  }

  def command(closure: => Unit): SimpleScalaCommandBuilder = {
    val child = new SimpleScalaCommandBuilder
    context.withValue(child)(closure)
    context.value.children ::= child
    child
  }

  def src: Source = new Source

  implicit class ArgWrapper(id: String) {

    def double: SingleArg[Double] = new GetOne[Double](id, global.doubleNum)

    def int: SingleArg[Int] = new GetOne[Int](id, global.integer)

    def bool: SingleArg[Boolean] = new GetOne[Boolean](id, global.bool)

    def raw: SingleArg[String] = new GetOne[String](id, global.remainingJoinedStrings)

    def str: SingleArg[String] = new GetOne[String](id, global.string)

    def vector3d: SingleArg[Vector3d] = new GetOne[Vector3d](id, global.vector3d)

    def location: SingleArg[Location[World]] = new GetOne[Location[World]](id, global.location)

    def player: ListArg[Player] with OrSource[Player] = new GetPlayer(id)

    def user: ListArg[User] with OrSource[User] = new GetUser(id)

    def world: ListArg[WorldProperties] = new GetList[WorldProperties](id, global.world)

    def dimension: ListArg[Dimension] = new GetList[Dimension](id, global.dimension)

    def literal: SingleArg[String] = new GetOne[String](id, global.literal(_, id))

    def literal(literals: String*): SingleArg[String] = new GetOne[String](id, global.literal(_, literals: _*))

    def catalog[T <: CatalogType](implicit ct: ClassTag[T]): ListArg[T] = {
      new GetList[T](id, global.catalogedElement(_, ct.runtimeClass.asInstanceOf[Class[T]]))
    }

    def flag: Flag = if (id.startsWith("-")) new Flag(List(id.substring(1))) else {
      throw new IllegalArgumentException("Flags must be started with a hyphen-minus")
    }
  }

  implicit class RegistryWrapper(plugin: AnyRef) {
    def registry: Registry = new Registry(plugin)
  }

  class Registry(plugin: AnyRef) {
    def +=(builder: SimpleScalaCommandBuilder): Unit = {
      impl.SimpleScalaCommandBuilderImpl.tasks += ((plugin, () => builder))
    }
  }

  private val context: DynamicVariable[SimpleScalaCommandBuilder] = new DynamicVariable(new SimpleScalaCommandBuilder)
}
