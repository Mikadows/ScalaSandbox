class ScastieCode {

  val nums: List[Int] = List(1, 55, 3, 4)
  nums.head
  /**
   * @param xs une liste d'entiers
   * @return L'élément max de `xs`
   * @throws java.util.NoSuchElementException si `xs` est une liste vide
   */
  def max(xs: List[Int]): Int = {
    xs match {
      case Nil => throw new NoSuchElementException
      case elem :: Nil => elem
      case elem1 ::elem2 :: rest => if (elem1 > elem2) {max(rest :+ elem1)} else { max(rest :+ elem2)}
    }
  }

  max(nums)

  /**
   * max of list itterative
   */
  def maxIt(xs: List[Int]): Int = {
    var maxValue = xs.head
    for (elem <- xs)
    {
      if (elem > maxValue) {
        maxValue = elem
      }
    }
    maxValue
  }

  maxIt(nums)


  /**
   * @param xs une liste d'entiers
   * @return La somme de tous les elements de `xs`
   */
  def sum(xs: List[Int]): Int = xs match {
    case Nil => throw new NoSuchElementException
    case e1 :: Nil => e1
    case e1 :: e2 :: rest => sum( (e1 + e2) :: rest)
  }


  sum(nums)
  sum((0 to 1000).toList)
  sum((0 to 1000000).toList)

  def sumIt(xs: List[Int]): Int = {
    var maxValue = 0
    for (elem <- xs)
    {
      maxValue += elem
    }
    maxValue
  }

  sumIt(nums)
  sumIt((0 to 1000).toList)
  sumIt((0 to 1000000).toList)

   def balance(chars: List[Char]): Boolean = {
     println(chars)
     chars match {
       case Nil => false
       // case '(' :: rest => balance(rest :+ '(')
       // case ')' :: rest => balance(rest :+ ')')
       // case _ => balance()
       case el :: rest => balance(rest)
     }

   }

   def balanceMod(chars: List[Char], acc: Int): Boolean = {
     // println(chars)
     chars match {
       case Nil => if (acc == 0) true else false
       case '(' :: rest => balanceMod(rest, acc +1)
       case ')' :: rest => if (acc > 0) balanceMod(rest, acc -1) else false
       case el :: rest => balanceMod(rest, acc)
     }

   }

   balanceMod("Comme répété avant, (Je le répète (encore et encore) à en perdre les mots).".toList, 0)
   balanceMod("Une liste doit être (une liste(et une liste) une)liste(j'ai dit) une liste".toList, 0)
   balanceMod("')boom(".toList, 0)
   balanceMod("j'aime bien(patrick(il est) sympa.".toList, 0)
   balanceMod(")()".toList, 0)


   balance("Comme répété avant, (Je le répète (encore et encore) à en perdre les mots).".toList)
   balance("Une liste doit être (une liste(et une liste) une)liste(j'ai dit) une liste)".toList)
   balance("')boom(".toList)
   balance("j'aime bien(patrick(il est) sympa.".toList)



  def countChange(valeur: Int, pieces: List[Int]): Int = {
    val sortedPiece = pieces.sortWith(_.compareTo(_) < 0)
    0


  }

  def countComb(valeur: Int, pieces: List[Int]): Int = {
    pieces match {
      case Nil => 0
//      case el :: Nil => if
    }
  }


  countChange(300,List(500,5,50,100,20,200,10))
  countChange(301,List(5,10,20,50,100,200,500))
}
