import com.knoldus.model.{Account, BinaryTree, Book, Employee}
import com.knoldus.pattermatch.PatternMatching
import org.scalatest.{Matchers, WordSpec}

class PatternMatchSpec extends WordSpec with Matchers {

  "wildcardPatternMatch function" should {
    "Be able to match against default value i.e wildcard if needed" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.wildCardPattern(Account(1234, "Hvdy09", 888808, "Bank of Baroda")) shouldEqual "It's an account"
    }
  }

  "constantPattern" should {
    "Be able to match against a constant" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.constantPattern(7) shouldEqual "Sunday... Enjoy"
    }
  }

  "variablePatternMatch" should {
    "Be able to match against a variable" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.variablePattern(Employee("Hina", 30000, "Librarian")) shouldEqual
        "This is Employee(Hina,30000.0,Librarian) and not a book."
    }
  }

  "height function" should {
    "Be able to match against constructor" in {
      val binaryTree = BinaryTree(1, Some(BinaryTree(0, Some(BinaryTree(1, Some(BinaryTree(1, None, None)), None)), None)),
        Some(BinaryTree(0, Some(BinaryTree(1, Some(BinaryTree(0, None, None)), None)), None)))
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.height(binaryTree) shouldEqual 4
    }
  }

  "sequencePattern" should {
    "Be able to match against a sequence" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.sequencePattern(List(1, 2, 3)) shouldEqual "List with second element 2"
    }
  }

  "tuplePattern" should {
    "Be able to match against a tuple" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.tuplePattern((1, 2, 3)) shouldEqual "This tuple has 3 elements"
    }
  }
  "typedPattern" should {
    "Be able to match against the type of the element" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.typedPattern(Book("Harry Potter", "J. K. Rowlings", 150)) shouldEqual "This is a book"
    }
  }

  "evenOdd" should {
    "Be able to find even and odd elements" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.evenOdd(List(1, 2, 3, 4, 5, 6, 7)) shouldEqual(List(6, 4, 2), List(7, 5, 3, 1))
    }
  }

  "findNoneOrSome" should {
    "Be able to find if there is element" in {
      val patternMatchSpec = new PatternMatching
      patternMatchSpec.findNoneOrSome(None) shouldEqual "no string provided"
    }
  }
}
