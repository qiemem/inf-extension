package org.nlogo.extensions.inf

import org.nlogo.api._
import Syntax._
import ScalaConversions._
import org.nlogo.agent

import scala.collection.mutable.WeakHashMap
import scala.collection.JavaConverters._

class InfExtension extends DefaultClassManager {
  import InfTopology._
  import PrimitiveConverters._
  def load(primitiveManager: PrimitiveManager) {
    primitiveManager.addPrimitive("zoom", InfTopology.zoom)
    primitiveManager.addPrimitive("center-xcor", centerXcor)
    primitiveManager.addPrimitive("center-ycor", centerYcor)
    primitiveManager.addPrimitive("to-inf-xcor", toInfXcor(_: Double))
    primitiveManager.addPrimitive("to-inf-ycor", toInfYcor(_: Double))
    primitiveManager.addPrimitive("to-inf-size", toInfSize(_: Double))
    primitiveManager.addPrimitive("to-view-xcor",toViewXcor(_: Double))
    primitiveManager.addPrimitive("to-view-ycor", toViewYcor(_: Double))
    primitiveManager.addPrimitive("to-view-size", toViewSize(_: Double))
    primitiveManager.addPrimitive("xcor", xcors)
    primitiveManager.addPrimitive("ycor", ycors)
    primitiveManager.addPrimitive("size", sizes)

    primitiveManager.addPrimitive("set-zoom", InfTopology.SetZoom)
    primitiveManager.addPrimitive("set-center", InfTopology.SetCenter)
    primitiveManager.addPrimitive("setxy", InfTopology.SetXY)
  }
}

object InfTopology {
  var zoom = 1.0
  var centerXcor = 0.0
  var centerYcor = 0.0

  // Apparently, it's quite difficult to create turtle variables in an
  // extension. Seth suggested using a WeakHashMap instead (weak so that the
  // the entries will be gced when the turtle dies).
  // https://groups.google.com/forum/?fromgroups#!topic/netlogo-devel/6if-otb2M_k
  val xcors = new WeakHashMap[Turtle, Double]() withDefaultValue 0.0
  val ycors = new WeakHashMap[Turtle, Double]() withDefaultValue 0.0
  val sizes = new WeakHashMap[Turtle, Double]() withDefaultValue 1.0

  def updateVisibility(turtle: agent.Turtle) = {
    val xcor = toViewXcor(xcors(turtle))
    val ycor = toViewYcor(ycors(turtle))
    val size = toViewSize(sizes(turtle))
    val w = turtle.world
    val minXcor = w.minPxcor - 0.5
    val maxXcor = w.maxPxcor + 0.5
    val minYcor = w.minPycor - 0.5
    val maxYcor = w.maxPycor + 0.5

    if (xcor < minXcor || maxXcor < xcor || ycor < minYcor || maxYcor < ycor) {
      turtle hidden true
    } else {
      turtle hidden false
      turtle.xandycor(xcor, ycor)
      turtle size size
    }
  }

  def updateVisibility(world: World): Unit =
    world.turtles.agents.asScala foreach { (a: Agent) =>
      updateVisibility(a.asInstanceOf[agent.Turtle])
    }


  def toInfXcor(viewXcor: Double): Double = viewXcor / zoom + centerXcor
  def toInfYcor(viewYcor: Double): Double = viewYcor / zoom + centerYcor
  def toInfSize(viewSize: Double): Double = viewSize / zoom
  def toViewXcor(infXcor: Double): Double = (infXcor - centerXcor) * zoom
  def toViewYcor(infYcor: Double): Double = (infYcor - centerYcor) * zoom
  def toViewSize(infSize: Double): Double = infSize * zoom

  object SetZoom extends DefaultCommand {
    override def getSyntax = commandSyntax(Array(NumberType))

    override def perform(args: Array[Argument], context: Context) = {
      val newZoom = args(0).getDoubleValue
      if (zoom != newZoom) {
        zoom = newZoom
        updateVisibility(context.getAgent.world)
      }
    }
  }

  object SetCenter extends DefaultCommand {
    override def getSyntax = commandSyntax(Array(NumberType, NumberType))

    override def perform(args: Array[Argument], context: Context) = {
      val centerX = args(0).getDoubleValue
      val centerY = args(1).getDoubleValue
      if (centerX != centerXcor || centerY != centerYcor) {
        centerXcor = centerX
        centerYcor = centerY
        updateVisibility(context.getAgent.world)
      }
    }
  }

  object SetXY extends Command {
    override def getSyntax =
      commandSyntax(Array(NumberType, NumberType))

    override def getAgentClassString = "T"
    override def getSwitchesBoolean = true

    override def perform(args: Array[Argument], context: Context) = {
      val turtle = context.getAgent.asInstanceOf[agent.Turtle]
      xcors(turtle) = args(0).getDoubleValue
      ycors(turtle) = args(1).getDoubleValue
      updateVisibility(turtle)
    }
  }
}

object PrimitiveConverters {
  implicit def double(getter: =>Double): Primitive = {
    object Getter extends DefaultReporter {
      override def report(args: Array[Argument], context: Context): AnyRef =
       getter: java.lang.Double
    }
    Getter
  }

  implicit def doubleDouble(func: (Double) => Double): Primitive = {
    object FuncReporter extends DefaultReporter {
      override def getSyntax() = reporterSyntax(Array(NumberType), NumberType)
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(args(0).getDoubleValue): java.lang.Double
    }
    FuncReporter
  }

  implicit def turtleDouble(func: (Turtle) => Double): Primitive = {
    object TurtleReporter extends DefaultReporter {
      override def getAgentClassString = "T"
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(context.getAgent.asInstanceOf[Turtle]): java.lang.Double
    }
    TurtleReporter
  }
}
