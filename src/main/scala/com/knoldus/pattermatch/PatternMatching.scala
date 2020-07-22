package com.knoldus.pattermatch


import com.knoldus.model._

class PatternMatching {
  /**
    * This basically show how wildcard pattern can be used. It is useful when you don't care about some cases. This can
    * be considered as default.
    *
    * @param account an object of type Account
    * @return
    */
  def wildCardPattern(account: Account): String = {
    account match {
      case Account(_, _, _, _) => "It's an account"
      case _ => "It is not an account"
    }
  }

  /**
    * This shows how constant can be matched.
    *
    * @param day an integer value for day
    * @return
    */
  def constantPattern(day: Int): String = {
    day match {
      case 1 => "Monday... Start your work "
      case 2 => "Tuesday"
      case 3 => "Wednesday"
      case 4 => "Thursday"
      case 5 => "Friday.. Partyyyy"
      case 6 => "Saturday... Relax"
      case 7 => "Sunday... Enjoy"
      case _ => "Not a day try, between 1-7"
    }
  }

  /**
    * This shows how we can match using variable pattern. This is same as using wildcard but in this the value can be
    * trapped and used later on.
    *
    * @param book an object of type Library
    * @return
    */
  def variablePattern(book: Library): String = {
    book match {
      case Book(name, author, price) => s"This $name is written by $author and sold at $price rupees."
      case otherObjectOfLibrary => s"This is $otherObjectOfLibrary and not a book."
    }
  }

  /**
    * This is an example of how Constructor match can be used. In constructor match the constructor of a class is used.
    * This will calculate the height of a binary tree.
    *
    * @param binaryTree an object of binary tree
    * @return
    */
  def height(binaryTree: BinaryTree): Int = {
    binaryTree match {
      case BinaryTree(_, Some(left), Some(right)) => 1 + max(height(left), height(right))
      case BinaryTree(_, Some(left), None) => 1 + height(left)
      case BinaryTree(_, None, Some(right)) => 1 + height(right)
      case _ => 1
    }
  }

  /**
    * This is an example of matching against a sequence.
    *
    * @param seq a sequence
    * @return
    */
  def sequencePattern(seq: Seq[Int]): String = {
    seq match {
      case List(_, 2, _*) => "List with second element 2"
      case List(0, _) => "List starts with 0 of length 2 "
      case _ => "Does not match other condition"
    }
  }

  /**
    * This is an example of matching against Tuple.
    *
    * @param tuple any tuple
    * @return
    */
  def tuplePattern(tuple: Any): String = {
    tuple match {
      case (elem1, elem2) => s"this tuple has following elements: $elem1, $elem2"
      case (_, _, _) => "This tuple has 3 elements"
      case _ => "Not a tuple as per requirements"
    }
  }

  /**
    * This demonstrates typed pattern. By using this we can match against the type of the element. This is a better
    * alternative for type test or cast.
    *
    * @param library an object of library
    * @return
    */
  def typedPattern(library: Library): String = {
    library match {
      case _: Book => "This is a book"
      case _: Employee => "This is an Employee"
    }
  }

  /**
    * This function shows the use of guard pattern. In guard pattern even after matching the case we can do some
    * conditional check and if the condition is true then only the related operation will be performed. This example
    * also shows how pattern matching can be used to extract the elements from the list.
    *
    * @param list any list of type Int
    * @return
    */
  def evenOdd(list: List[Int]): (List[Int], List[Int]) = {
    def inner(list: List[Int], even: List[Int], odd: List[Int]): (List[Int], List[Int]) = {
      list match {
        case first :: rest if first % 2 == 0 => inner(rest, first :: even, odd)
        case first :: rest if first % 2 != 0 => inner(rest, even, first :: odd)
        case Nil => (even, odd)
      }
    }

    inner(list, List.empty[Int], List.empty[Int])
  }

  /**
    * This function is an example of how the value can be extracted from Option. It also shows how pattern matching can
    * be done on Sealed class. By pattern matching on sealed class, it's easy to cover all the possible cases and it
    * does not need default case.
    *
    * @param string a string
    * @return
    */
  def findNoneOrSome(string: Option[String]): String = {
    string match {
      case Some(str) => s"$str"
      case None => "no string provided"
    }
  }

  private def max(i: Int, i1: Int): Int = {
    if (i > i1) i else i1
  }

}
