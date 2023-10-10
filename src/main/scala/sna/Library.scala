/* Copyright (c) 2014 Sanjay Dasgupta, All Rights Reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */

/*
 * Added logging
 * 
 * Xoan C. Pardo
 */

package sna

import com.sun.jna.{Function => JNAFunction}

import com.typesafe.scalalogging._

import scala.language.dynamics
import scala.collection.mutable

class Library (val libName: String) extends Dynamic with LazyLogging {

  logger.trace(s"Created Library: $libName")

  class Invocation (val jnaFunction: JNAFunction, val args: Array[Object]) {
    def apply[R](implicit m: Manifest[R]): R = {
      if (m.runtimeClass == classOf[Unit]) {
        jnaFunction.invoke(args).asInstanceOf[R]
      } else {
        jnaFunction.invoke(m.runtimeClass, args).asInstanceOf[R]
      }
    }
    def as[R](implicit m: Manifest[R]) = apply[R](m)
    def asInstanceOf[R](implicit m: Manifest[R]) = apply[R](m)
  }

  def applyDynamic(functionName: String)(args: Any*): Invocation = {
    logger.trace(s"LibName: $libName, FunctionName: $functionName")
    var jnaFunction: JNAFunction = null
    if (functionCache.contains(functionName)) {
      jnaFunction = functionCache(functionName)
      logger.trace("cache jnaFunction: " + jnaFunction)
    } else {
      jnaFunction = JNAFunction.getFunction(libName, functionName)
      logger.trace("jnaFunction: " + jnaFunction)
      functionCache(functionName) = jnaFunction
      logger.trace("FunctionCache: " + functionCache.toString)
    }
    new Invocation(jnaFunction, args.map(_.asInstanceOf[Object]).toArray[Object])
  }

  private val functionCache = mutable.Map.empty[String, JNAFunction]
}


object Library /*extends App*/ {
  def apply(libName: String) = new Library(libName)
  /*
  //  Test code ...
  //
  val myLib = Library("c")
  println(myLib.atol("0345")[Int])
  println(myLib.malloc(1000)[Long])
  myLib.printf("[printf] clock: %ld\n", myLib.clock().asInstanceOf[Int]).as[Unit]
  myLib.printf("value: %d %f", 123, 3.12).as[Unit]
  * 
  */
}
