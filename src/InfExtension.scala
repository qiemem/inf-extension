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
    primitiveManager.addPrimitive("to-view-xcor", toViewXcor(_: Double))
    primitiveManager.addPrimitive("to-view-ycor", toViewYcor(_: Double))
    primitiveManager.addPrimitive("to-view-size", toViewSize(_: Double))
    primitiveManager.addPrimitive("xcor", xcors)
    primitiveManager.addPrimitive("ycor", ycors)
    primitiveManager.addPrimitive("size", sizes)

    primitiveManager.addPrimitive("set-zoom", setZoom(_: World, _: Double))
    primitiveManager.addPrimitive("set-center", setCenter(_: World, _: Double, _: Double))
    primitiveManager.addPrimitive("setxy", setXY(_: Turtle, _: Double, _: Double))
    primitiveManager.addPrimitive("set-xcor", setXcor(_: Turtle, _: Double))
    primitiveManager.addPrimitive("set-ycor", setYcor(_: Turtle, _: Double))
    primitiveManager.addPrimitive("set-size", setSize(_: Turtle, _: Double))
    primitiveManager.addPrimitive("forward", forward(_: Turtle, _: Double))
    primitiveManager.addPrimitive("fd", forward(_: Turtle, _: Double))
    primitiveManager.addPrimitive("distancexy", distanceXY(_: Turtle, _: Double, _: Double))
    primitiveManager.addPrimitive("distance", distance(_: Turtle, _: Turtle))
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

  def updateVisibility(turtle: Turtle) = {
    val t = turtle.asInstanceOf[agent.Turtle]
    val xcor = toViewXcor(xcors(t))
    val ycor = toViewYcor(ycors(t))
    val size = toViewSize(sizes(t))
    val w = t.world
    val minXcor = w.minPxcor - 0.5
    val maxXcor = w.maxPxcor + 0.5
    val minYcor = w.minPycor - 0.5
    val maxYcor = w.maxPycor + 0.5

    if (xcor < minXcor || maxXcor < xcor || ycor < minYcor || maxYcor < ycor) {
      t hidden true
    } else {
      t hidden false
      t.xandycor(xcor, ycor)
      t size size
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

  def setZoom(w: World, z: Double) = if (z != zoom) {
    zoom = z
    updateVisibility(w)
  }

  def setCenter(w: World, x: Double, y: Double) = if (x != centerXcor || y != centerYcor) {
    centerXcor = x
    centerYcor = y
    updateVisibility(w)
  }

  def setXY(t: Turtle, x: Double, y: Double) = {
    xcors(t) = x
    ycors(t) = y
    updateVisibility(t)
  }

  def setXcor(t: Turtle, x: Double) = {
    xcors(t) = x
    updateVisibility(t)
  }

  def setYcor(t: Turtle, y: Double) = {
    ycors(t) = y
    updateVisibility(t)
  }

  def setSize(t: Turtle, s: Double) = {
    sizes(t) = s
    updateVisibility(t)
  }

  def forward(turtle: Turtle, dist: Double) = {
    val t = turtle.asInstanceOf[agent.Turtle]
    setXY(t, t.dx() * dist + xcors(t), t.dy() * dist + ycors(t))
  }

  def distanceXY(turtle: Turtle, x: Double, y: Double): Double = {
    val dx = xcors(turtle) - x
    val dy = xcors(turtle) - y
    StrictMath.sqrt(dx * dx + dy * dy)
  }

  def distance(turtle: Turtle, other: Turtle): Double = {
    distanceXY(turtle, xcors(other), ycors(other))
  }
}

object PrimitiveConverters {
  implicit def reporterDouble(getter: =>Double): Reporter = {
    object Getter extends DefaultReporter {
      override def report(args: Array[Argument], context: Context): AnyRef =
       getter: java.lang.Double
    }
    Getter
  }

  implicit def reporterDoubleDouble(func: (Double) => Double): Reporter = {
    object FuncReporter extends DefaultReporter {
      override def getSyntax = reporterSyntax(Array(NumberType), NumberType)
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(args(0).getDoubleValue): java.lang.Double
    }
    FuncReporter
  }

  implicit def reporterTurtleDouble(func: (Turtle) => Double): Reporter = {
    object TurtleReporter extends DefaultReporter {
      override def getAgentClassString = "T"
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(context.getAgent.asInstanceOf[Turtle]): java.lang.Double
    }
    TurtleReporter
  }

  implicit def reporterTurtleTurtleDouble(func: (Turtle, Turtle) => Double): Reporter = {
    object TurtleReporter extends DefaultReporter {
      override def getAgentClassString = "T"
      override def getSyntax = reporterSyntax(Array(TurtleType), NumberType)
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(context.getAgent.asInstanceOf[Turtle], args(0).getAgent.asInstanceOf[Turtle]): java.lang.Double
    }
    TurtleReporter
  }

  implicit def reporterTurtleDoubleDoubleDouble(func: (Turtle, Double, Double) => Double): Reporter = {
    object TurtleReporter extends DefaultReporter {
      override def getSyntax = reporterSyntax(Array(NumberType, NumberType), NumberType)
      override def getAgentClassString = "T"
      override def report(args: Array[Argument], context: Context): AnyRef =
        func(context.getAgent.asInstanceOf[Turtle], args(0).getDoubleValue, args(1).getDoubleValue): java.lang.Double
    }
    TurtleReporter
  }


  implicit def commandWorldDouble(func: (World, Double) => Unit): Command ={
    object WorldCommand extends DefaultCommand {
      override def getSyntax = commandSyntax(Array(NumberType))
      override def perform(args: Array[Argument], context: Context) =
        func(context.getAgent.world, args(0).getDoubleValue)
    }
    WorldCommand
  }

  implicit def commandWorldDoubleDouble(func: (World, Double, Double) => Unit): Command ={
    object WorldCommand extends DefaultCommand {
      override def getSyntax = commandSyntax(Array(NumberType, NumberType))
      override def perform(args: Array[Argument], context: Context) =
        func(context.getAgent.world, args(0).getDoubleValue, args(1).getDoubleValue)
    }
    WorldCommand
  }

  implicit def commandTurtleDouble(func: (Turtle, Double) => Unit): Command ={
    object TurtleCommand extends DefaultCommand {
      override def getAgentClassString = "T"
      override def getSwitchesBoolean = true
      override def getSyntax = commandSyntax(Array(NumberType))
      override def perform(args: Array[Argument], context: Context) =
        func(context.getAgent.asInstanceOf[Turtle], args(0).getDoubleValue)
    }
    TurtleCommand
  }

  implicit def commandTurtleDoubleDouble(func: (Turtle, Double, Double) => Unit): Command ={
    object TurtleCommand extends DefaultCommand {
      override def getAgentClassString = "T"
      override def getSyntax = commandSyntax(Array(NumberType, NumberType))
      override def getSwitchesBoolean = true
      override def perform(args: Array[Argument], context: Context) =
        func(context.getAgent.asInstanceOf[Turtle], args(0).getDoubleValue, args(1).getDoubleValue)
    }
    TurtleCommand
  }
}
