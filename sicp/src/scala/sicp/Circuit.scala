package scala.sicp

import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object Circuit {
  
  val theAgenda = new Agenda()
  val inverterDelay = 2
  val andDelay = 3
  val orDelay = 5
  
  type Action = () => Unit
  type Segment = Queue[Event]
  
  class Wire(var signal: Boolean = false, 
    var actions:List[Action] = List[Action]()) {
    
    def get = signal
    
    def set(value:Boolean) = {
      if(value != signal) {
        signal = value
        actions.foreach(_.apply)
      }
    }
    
    def add(a: Action) = {
      actions = a :: actions
      a.apply
    }
  }
  
  case class Event(
      val time: Int, 
      val action: Action)
  
  class Agenda(
      var events:Map[Int, Segment] = Map.empty[Int, Segment], 
      var time:Int = 0) {
    
    def isEmpty = events.isEmpty
    
    def first:Event = {
      val e = events(events.keys.min).head
      time = e.time
      e
    }
    
    def remove = {
      val q = events(events.keys.min)
      val e = q.dequeue

      if(q.isEmpty) 
        events -= e.time
    }
    
    def add(e: Event) = {
      if (!events.contains(e.time))
      	events += (e.time -> Queue.empty[Event])
      
      val q = events(e.time)
      q.enqueue(e)
    }
  }
  
  def afterDelay(delay:Int, action: Action) = 
    theAgenda.add(Event(delay + theAgenda.time, action))
  
  def or(in1:Wire, in2:Wire, out:Wire) = {
    val fun = () => {
      val newValue = in1.get || in2.get 
      afterDelay(orDelay, () => {
        out.set(newValue)
        })
    }
    in1.add(fun)
    in2.add(fun)
  }
  
  def and(in1:Wire, in2:Wire, out:Wire) = {
    val fun = () => {
      val newValue = in1.get && in2.get
      afterDelay(andDelay, () => {
        out.set(newValue)
        })
    }
    in1.add(fun)
    in2.add(fun)
  }
  
  def inverter(in:Wire, out:Wire) = {
	  in.add(() => {
	    val newValue = !in.get
	    afterDelay(inverterDelay, () => {
	      //System.out.println("Inverter => out " + theAgenda.time + ", New value: " + newValue)
	      out.set(newValue)})
	  })
  }
  
  def halfAdder(a:Wire, b:Wire, s:Wire, c:Wire) = {
    val d = new Wire
    val e = new Wire
    
    or(a, b, d)
    and(a, b, c)
    inverter(c, e)
    and(d, e, s)
  }

  def fullAdder(a:Wire, b:Wire, cIn:Wire, sum:Wire, cOut:Wire) = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    
    halfAdder(b, cIn, s, c1)
    halfAdder(a, s, sum, c2)
    
    or(c1, c2, cOut)
  }
  
  def propagate():Unit = 
    if (!theAgenda.isEmpty) {
      theAgenda.first.action.apply
      theAgenda.remove
      propagate
    }
  
  def probe(name: String, wire: Wire) = {
    wire.add(()=> {
      System.out.println(name + " " + theAgenda.time + ", New value: " + wire.get)
    })
  }
    	
  
  def main(args: Array[String]): Unit = {

	  val input1 = new Wire
	  val input2 = new Wire
	  val sum = new Wire
	  val carry = new Wire
	  
	  //probe("Input1", input1)
	  //probe("Input2", input2)
	  probe("Sum", sum)
	  probe("Carry", carry)
	  
	  halfAdder(input1, input2, sum, carry)

	  input1.set(true)
	  propagate
	  input2.set(true)
	  propagate
	  
  }

}