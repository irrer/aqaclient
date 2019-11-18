package org.aqaclient

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

class HelloActor extends Actor {

  println("Constructed HelloActor")
  var t = System.currentTimeMillis
  def showT = {
    val cur = System.currentTimeMillis
    val elapsed = cur - t
    println("HelloActor elapsed: " + elapsed)
    t = cur
  }

  def receive = {
    case "hello" => {
      println("hello back at you")
      Thread.sleep(200)
      showT
    }
    case thing: Any => {
      println("huh? : " + thing.getClass + " : " + thing.toString)
      showT
    }
  }
}

object Acting extends App {

  val system = ActorSystem("HelloSystem")
  // default Actor constructor
  val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  var t = System.currentTimeMillis
  def showT = {
    val cur = System.currentTimeMillis
    val elapsed = cur - t
    println("Acting elapsed: " + elapsed)
    t = cur
  }
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hellox"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "buenos dias"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  helloActor ! "hello"; showT
  //  }

  Thread.sleep(5000)
  println("Exiting...")
  System.exit(0)
}