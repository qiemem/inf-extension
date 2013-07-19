package org.nlogo.extensions.inf

import org.nlogo.api._
import Syntax._
import ScalaConversions._
import org.nlogo.agent

import scala.collection.mutable.WeakHashMap
import scala.ref.WeakReference
import scala.collection.JavaConverters._

class InfExtension extends DefaultClassManager {
  def load(primitiveManager: PrimitiveManager) {
    import InfTopology._
    import PrimitiveConverters._
    // Global reporters
    primitiveManager.addPrimitive("zoom", zoom)
    primitiveManager.addPrimitive("center-xcor", centerXcor)
    primitiveManager.addPrimitive("center-ycor", centerYcor)
    primitiveManager.addPrimitive("to-inf-xcor", toInfXcor _)
    primitiveManager.addPrimitive("to-inf-ycor", toInfYcor _)
    primitiveManager.addPrimitive("to-inf-size", toInfSize _)
    primitiveManager.addPrimitive("to-view-xcor", toViewXcor _)
    primitiveManager.addPrimitive("to-view-ycor", toViewYcor _)
    primitiveManager.addPrimitive("to-view-size", toViewSize _)

    // Global commands
    primitiveManager.addPrimitive("set-zoom", setZoom _)
    primitiveManager.addPrimitive("set-center", setCenter _)

    // Turtle reporters
    primitiveManager.addPrimitive("xcor", xcors)
    primitiveManager.addPrimitive("ycor", ycors)
    primitiveManager.addPrimitive("size", sizes)
    primitiveManager.addPrimitive("distancexy", distanceXY _)
    primitiveManager.addPrimitive("distance", distance _)
    primitiveManager.addPrimitive("towardsxy", towardsXY _)
    primitiveManager.addPrimitive("towards", towards _)
    primitiveManager.addPrimitive("in-radius", inRadius _)

    // Turtle commands
    primitiveManager.addPrimitive("setxy", setXY _)
    primitiveManager.addPrimitive("set-xcor", setXcor _)
    primitiveManager.addPrimitive("set-ycor", setYcor _)
    primitiveManager.addPrimitive("set-size", setSize _)
    primitiveManager.addPrimitive("forward", forward _)
    primitiveManager.addPrimitive("fd", forward _)
    primitiveManager.addPrimitive("backward", backward _)
    primitiveManager.addPrimitive("bk", backward _)
    primitiveManager.addPrimitive("facexy", faceXY _)
    primitiveManager.addPrimitive("face", face _)
  }
}

@annotation.strictfp
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

  // For tracking pen state when the turtle is out of the view
  val penMode = new WeakHashMap[Turtle, String]() withDefaultValue agent.Turtle.PEN_UP

  val inView = new WeakHashMap[Turtle, Boolean]() withDefaultValue true

  // TODO: Come up with more sensible parameters here
  val rootsTable = new agent.RootsTable(100, 100)

  def ensureTurtleAgent(turtle: Turtle) = turtle match {
    case t: agent.Turtle => t
    case _ => throw new ExtensionException("Invalid turtle found: " + turtle.toString)
  }

  def updateVisibility(turtle: agent.Turtle) {
    val xcor = toViewXcor(xcors(turtle))
    val ycor = toViewYcor(ycors(turtle))
    val size = toViewSize(sizes(turtle))
    val w = turtle.world
    val minXcor = w.minPxcor - 0.5  // inclusive
    val maxXcor = w.maxPxcor + 0.5  // exclusive
    val minYcor = w.minPycor - 0.5  // inclusive
    val maxYcor = w.maxPycor + 0.5  // exclusive

    if (xcor < minXcor || maxXcor <= xcor || ycor < minYcor || maxYcor <= ycor) {
      if (inView(turtle)) {
        penMode(turtle) = turtle.penMode
        turtle penMode agent.Turtle.PEN_UP;
        turtle hidden true
        inView(turtle) = false
      }
    } else {
      turtle.xandycor(xcor, ycor)
      turtle size size
      if (!inView(turtle)) {
        turtle hidden false
        turtle penMode penMode(turtle)
        inView(turtle) = true
      }
    }
  }

  def updateVisibility(world: World) {
    world.turtles.agents.asScala foreach {
      case t: agent.Turtle => updateVisibility(t)
    }
  }

  def toInfXcor(viewXcor: Double): Double = viewXcor / zoom + centerXcor
  def toInfYcor(viewYcor: Double): Double = viewYcor / zoom + centerYcor
  def toInfSize(viewSize: Double): Double = viewSize / zoom
  def toViewXcor(infXcor: Double): Double = (infXcor - centerXcor) * zoom
  def toViewYcor(infYcor: Double): Double = (infYcor - centerYcor) * zoom
  def toViewSize(infSize: Double): Double = infSize * zoom

  def setZoom(w: World, z: Double) {
    if (z != zoom) {
      zoom = z
      updateVisibility(w)
    }
  }

  def setCenter(w: World, x: Double, y: Double) {
    if (x != centerXcor || y != centerYcor) {
      centerXcor = x
      centerYcor = y
      updateVisibility(w)
    }
  }

  def setXY(t: Turtle, x: Double, y: Double) {
    xcors(t) = x
    ycors(t) = y
    updateVisibility(ensureTurtleAgent(t))
  }

  def setXcor(t: Turtle, x: Double) {
    xcors(t) = x
    updateVisibility(ensureTurtleAgent(t))
  }

  def setYcor(t: Turtle, y: Double) {
    ycors(t) = y
    updateVisibility(ensureTurtleAgent(t))
  }

  def setSize(t: Turtle, s: Double) {
    sizes(t) = s
    updateVisibility(ensureTurtleAgent(t))
  }

  def forward(turtle: Turtle, dist: Double) {
    val t = ensureTurtleAgent(turtle)
    setXY(t, t.dx() * dist + xcors(t), t.dy() * dist + ycors(t))
  }

  def backward(turtle: Turtle, dist: Double) {
    forward(turtle, -dist)
  }

  def distanceSq(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val dx = x2 - x1
    val dy = y2 - y1
    dx * dx + dy * dy
  }

  def distance(x1: Double, y1: Double, x2: Double, y2: Double): Double =
    rootsTable gridRoot distanceSq(x1, y1, x2, y2)

  def distanceXY(turtle: Turtle, x: Double, y: Double): Double =
    distance(xcors(turtle), ycors(turtle), x, y)

  def distance(turtle: Turtle, other: Turtle): Double =
    distanceXY(turtle, xcors(other), ycors(other))

  def inRadius(turtle: Turtle, agents: AgentSet, radius: Double): AgentSet = {
    // FIXME: This is really awful, but I can't figure out how to make an
    // AgentSet without using agent.* stuff
    val result = (agents.agents.asScala withFilter {
      case t: Turtle => distance(turtle, t) <= radius
      case _ => throw new ExtensionException("in-radius only works on turtle-sets.")
    } map {
      case a: agent.Agent => a
      case a => throw new ExtensionException("Invalid agent found: " + a.toString)
    }).toArray
    turtle.world match {
      case w: agent.World => new agent.ArrayAgentSet(classOf[agent.Turtle], result , w)
      case _ => throw new ExtensionException("Invalid world found: " + turtle.world.toString)
    }
  }


  def towardsXY(turtle: Turtle, x: Double, y: Double): Double = {
    val dx = x - xcors(turtle)
    val dy = y - ycors(turtle)
    // See org.nlogo.agent.Protractor#towards(double, double, double, double, boolean)
    (270 + StrictMath.toDegrees(StrictMath.PI + StrictMath.atan2(-dy, dx))) % 360
  }

  def towards(turtle: Turtle, other: Turtle): Double =
    towardsXY(turtle, xcors(other), ycors(other))

  def faceXY(turtle: Turtle, x: Double, y: Double) {
    turtle heading towardsXY(turtle, x, y)
  }

  def face(turtle: Turtle, other: Turtle) {
    turtle heading towards(turtle, other)
  }


  object QuadTree {
    val MaxTurtles = 8;

    def apply(turtles: Seq[Turtle]): QuadTree = {
      val xs   = turtles map xcors
      val ys   = turtles map ycors
      val minX = xs.min
      val minY = ys.min
      val maxX = xs.max
      val maxY = ys.max
      val size = scala.math.max(maxX - minX, maxY - minY) / 2
      QuadTree(maxX - minX / 2, maxY - minY / 2, size, turtles)
    }

    def apply(x: Double, y: Double, size: Double, turtles: Seq[Turtle]): QuadTree = {
      // in case a bunch are on the same place
      if ((turtles map { t: Turtle => (xcors(t), ycors(t)) }).toSet.size > MaxTurtles) {
        val childSize = size / 2
        val (nwTurtles, neTurtles, swTurtles, seTurtles) = divide(x, y, turtles)
        val nw = QuadTree(x - childSize, y + childSize, childSize, nwTurtles)
        val ne = QuadTree(x + childSize, y + childSize, childSize, neTurtles)
        val sw = QuadTree(x - childSize, y - childSize, childSize, swTurtles)
        val se = QuadTree(x + childSize, y - childSize, childSize, seTurtles)
        new QuadBranch(nw, ne, sw, se)
      } else {
        new QuadLeaf(x, y, size, turtles map { new WeakReference(_) })
      }
    }

    /**
     * Divides the given turtles into four quadrants using (x,y) as the origin.
     * @param x The x coordinate the turtles will be divided around
     * @param y The y coordinate the turtles will be divided around
     * @return (nw, ne, sw, se)
     */
    def divide(x: Double, y: Double, turtles: Seq[Turtle]): (Seq[Turtle], Seq[Turtle], Seq[Turtle], Seq[Turtle]) = (
        turtles filter { t: Turtle => xcors(t) <  x && ycors(t) >= y }
      , turtles filter { t: Turtle => xcors(t) >= x && ycors(t) >= y }
      , turtles filter { t: Turtle => xcors(t) <  x && ycors(t) <  y }
      , turtles filter { t: Turtle => xcors(t) >= x && ycors(t) <  y }
    )

  }

  abstract class QuadTree(val x: Double, val y: Double, val size: Double) {
    def maxX = x + size   // exclusive
    def minX = x - size   // inclusive
    def maxY = y + size   // exclusive
    def minY = y - size   // inclusive

    def contains(turtle: Turtle): Boolean = contains(xcors(turtle), ycors(turtle))

    def contains(x: Double, y: Double): Boolean =
      minX <= x && x < maxX && minY <= y && y < maxY

    def possibleOverlap(x: Double, y: Double, r: Double): Boolean =
      minX <= x + r && x - r < maxX && minY <= y + r && y - r < maxY

    def inRadius(x: Double, y: Double, r: Double): TraversableOnce[Turtle]

    def count: Int
  }

  class QuadBranch(val nw: QuadTree, val ne: QuadTree, val sw: QuadTree, val se: QuadTree)
      extends QuadTree(sw.x, sw.y, sw.size*2) {

    val children = Seq(nw, ne, sw, ne)

    def count: Int = (children map { _.count }).sum
    def inRadius(x: Double, y: Double, r: Double): TraversableOnce[Turtle] =
      children filter { _.possibleOverlap(x, y, r) } flatMap { _.inRadius(x, y, r) }
  }

  class QuadLeaf(x: Double, y: Double, size: Double, val turtles: Seq[WeakReference[Turtle]])
      extends QuadTree(x, y, size) {
    def livingTurtles: Seq[Turtle] = (turtles map { _.get }).flatten

    def count: Int = livingTurtles.length

    def inRadius(x: Double, y: Double, r: Double): TraversableOnce[Turtle] =
      livingTurtles filter { t: Turtle => distanceXY(t, x, y) < r }
  }

}

object PrimitiveConverters {
  def asTurtle(a: Agent): Turtle = a match {
    case t: Turtle => t
    case _ => throw new ExtensionException("Turtle required. You gave " + a.toString)
  }

  implicit def reporterDouble(getter: => Double): Reporter = new DefaultReporter {
    override def report(args: Array[Argument], context: Context): AnyRef =
      getter: java.lang.Double
  }

  implicit def reporterDoubleDouble(func: (Double) => Double): Reporter = new DefaultReporter {
    override def getSyntax = reporterSyntax(Array(NumberType), NumberType)
    override def report(args: Array[Argument], context: Context): AnyRef =
      func(args(0).getDoubleValue): java.lang.Double
  }

  implicit def reporterTurtleDouble(func: (Turtle) => Double): Reporter = new DefaultReporter {
    override def getAgentClassString = "T"
    override def report(args: Array[Argument], context: Context): AnyRef =
      func(asTurtle(context.getAgent)): java.lang.Double
  }

  implicit def reporterTurtleTurtleDouble(func: (Turtle, Turtle) => Double): Reporter = new DefaultReporter {
    override def getAgentClassString = "T"
    override def getSyntax = reporterSyntax(Array(TurtleType), NumberType)
    override def report(args: Array[Argument], context: Context): AnyRef =
      func(asTurtle(context.getAgent), asTurtle(args(0).getAgent)): java.lang.Double
  }

  implicit def reporterTurtleDoubleDoubleDouble(func: (Turtle, Double, Double) => Double): Reporter = new DefaultReporter {
    override def getSyntax = reporterSyntax(Array(NumberType, NumberType), NumberType)
    override def getAgentClassString = "T"
    override def report(args: Array[Argument], context: Context): AnyRef =
      func(asTurtle(context.getAgent), args(0).getDoubleValue, args(1).getDoubleValue) : java.lang.Double
  }

  implicit def reporterTurtleTurtlesetDoubleAgentSet(func: (Turtle, AgentSet, Double) => AgentSet): Reporter = new DefaultReporter {
    override def getSyntax = reporterSyntax(TurtlesetType, Array(NumberType), TurtlesetType, NormalPrecedence + 2, false)
    override def getAgentClassString = "T"
    override def report(args: Array[Argument], context: Context): AnyRef =
      func(asTurtle(context.getAgent), args(0).getAgentSet, args(1).getDoubleValue)

  }

  implicit def commandWorldDouble(func: (World, Double) => Unit): Command = new DefaultCommand {
    override def getSyntax = commandSyntax(Array(NumberType))
    override def perform(args: Array[Argument], context: Context) =
      func(context.getAgent.world, args(0).getDoubleValue)
  }

  implicit def commandWorldDoubleDouble(func: (World, Double, Double) => Unit): Command = new DefaultCommand {
    override def getSyntax = commandSyntax(Array(NumberType, NumberType))
    override def perform(args: Array[Argument], context: Context) =
      func(context.getAgent.world, args(0).getDoubleValue, args(1).getDoubleValue)
  }

  implicit def commandTurtleDouble(func: (Turtle, Double) => Unit): Command = new DefaultCommand {
    override def getAgentClassString = "T"
    override def getSwitchesBoolean = true
    override def getSyntax = commandSyntax(Array(NumberType))
    override def perform(args: Array[Argument], context: Context) =
      func(asTurtle(context.getAgent), args(0).getDoubleValue)
  }

  implicit def commandTurtleTurtle(func: (Turtle, Turtle) => Unit): Command = new DefaultCommand {
    override def getAgentClassString = "T"
    override def getSwitchesBoolean = true
    override def getSyntax = commandSyntax(Array(TurtleType))
    override def perform(args: Array[Argument], context: Context) =
      func(asTurtle(context.getAgent), asTurtle(args(0).getAgent))
  }

  implicit def commandTurtleDoubleDouble(func: (Turtle, Double, Double) => Unit): Command = new DefaultCommand {
    override def getAgentClassString = "T"
    override def getSyntax = commandSyntax(Array(NumberType, NumberType))
    override def getSwitchesBoolean = true
    override def perform(args: Array[Argument], context: Context) =
      func(asTurtle(context.getAgent), args(0).getDoubleValue, args(1).getDoubleValue)
  }
}
