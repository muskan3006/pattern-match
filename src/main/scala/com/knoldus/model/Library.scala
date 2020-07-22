package com.knoldus.model

trait Library

case class Employee(name:String,salary:Double,designation:String) extends Library
case class Book(name:String,author:String,price:Int) extends Library
