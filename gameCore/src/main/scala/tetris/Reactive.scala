package tetris

import scala.util.{DynamicVariable, Try}

class Signal[T](expr: => T){
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)
  def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  import Signal._
  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue ){
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "Cyclic signal definition")
    myValue
  }
}

object Signal{
  private val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T): Signal[T] = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???){
  override def computeValue(): Unit = ()
}

class Var[T](expr: => T) extends Signal[T](expr){
  override def update(expr: => T): Unit = super.update(expr)
}

object Var{
  def apply[T](expr: => T): Var[T] = new Var(expr)
}

//class StackableVariable[T](init: T){
//  private var values: List[T] = List(init)
//  def value: T = values.head
//  def withValue[R](newValue: T)(op: => R): R ={
//    values = newValue :: values
//    try op finally values = values.tail
//  }
//}