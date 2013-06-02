package org.nlogo.extensions.inf

import org.nlogo.api._
import Syntax._
import ScalaConversions._
import org.nlogo.agent

import scala.collection.mutable.WeakHashMap
import scala.collection.JavaConverters._

class InfExtension extends DefaultClassManager {
  def load(primitiveManager: PrimitiveManager) {
    primitiveManager.addPrimitive("set-zoom", InfTopology.SetZoom)
    primitiveManager.addPrimitive("set-center", InfTopology.SetCenter)
    primitiveManager.addPrimitive("zoom", InfTopology.Zoom)
    primitiveManager.addPrimitive("center-xcor", InfTopology.CenterXcor)
    primitiveManager.addPrimitive("center-ycor", InfTopology.CenterYcor)
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
    val xcor = (xcors(turtle) - centerXcor) * zoom
    val ycor = (ycors(turtle) - centerYcor) * zoom
    val size = sizes(turtle) * zoom
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

  object Zoom extends DefaultReporter {
    override def report(args: Array[Argument], context: Context): AnyRef =
      zoom: java.lang.Double
  }
  object CenterXcor extends DefaultReporter {
    override def report(args: Array[Argument], context: Context): AnyRef =
      centerXcor: java.lang.Double
  }
  object CenterYcor extends DefaultReporter {
    override def report(args: Array[Argument], context: Context): AnyRef =
      centerYcor: java.lang.Double
  }

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