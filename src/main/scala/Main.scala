object Main {
  def main(args: Array[String]): Unit = {
//    val xs = List(1, 50, 42, 63, 25)
//    println(max(xs))
//    println(sum(xs))
//    trianglePascal(3)
//    println(sumOfMax(xs))
//    println(balance("')boom(".toList, 0))
//    println(balance("Comme répété avant, (Je le répète (encore et encore) à en perdre les mots).".toList) )
//    println(balance("Une liste doit être (une liste(et une liste) une)liste(j'ai dit) une liste)".toList))
//    println(balance("')boom(".toList))
//    println(balance("j'aime bien(patrick(il est) sympa.".toList))

//    val john = PrimaryStudent("John", 8)
//    val jill = PrimaryStudent("Jill", 10)
//    val james = SecondaryStudent("James", 13)
//    val joe = SecondaryStudent("Joe", 14)
//    val jack = SecondaryStudent("Jack", 11)
//
//    val students = List(john, joe, jill, james,jack)
//    listStudents(students)

    val tree = Node(
                Node(
                  Leaf(12),
                  Node(
                    Node(Leaf(5), Leaf(10))
                    , Leaf(4)
                  )
                ),
                Node(Leaf(1), Leaf(2))
              )
    val subTree =                 Node(
      Node(Leaf(5), Leaf(10))
      , Leaf(4)
    )

    println("Count node "+ countNode(tree))
    println("Max Length "+ maxLength(tree))

    println("Is sub tree "+ tree.isSubTree(subTree))
  }

  def balance(chars: List[Char], nbPar: Int = 0) : Boolean = chars match {
    case Nil => nbPar == 0
    case ')' :: rest => if (nbPar <= 0) false else balance(rest, nbPar-1)
    case '(':: rest => balance(rest, nbPar+1)
    case _ :: rest => balance(rest, nbPar)
  }

  def trianglePascal(n: Int): Unit = {
    if (n < 0) return
    trianglePascal(n-1)
    for ( i <- 0 to n+1) {
      print(pascal(i, n))
    }
    println()
  }

  def pascal(col: Int, line: Int): Int = {
    if(col == 0 || line == 0) 1
    else if(col > line) 1
    else pascal(col, line-1) + pascal(col-1, line-1)
  }

  /**
   *   Proposer une implémentation récursive de la fonction suivante qui retourne le maximum des éléments d’une liste:
   * @param xs une liste d'entiers
   * @return L'élément max de `xs`
   * @throws java.util.NoSuchElementException si `xs` est une liste vide
   */
  def max(xs: List[Int]): Int = xs match {
    case Nil => throw new NoSuchElementException
    case el1 :: el2 :: rest => if (el1 > el2 ) max(el1::rest) else max(el2::rest)
    case el :: Nil => el
  }

  /**
   * Proposer une implémentation récursive la fonction suivante qui calcule la somme des éléments contenue dans une liste xs.
   * @param xs une liste d'entiers
   * @return La somme de tous les elements de `xs`
   */
  def sum(xs: List[Int]): Int = xs match {
    case Nil => throw new NoSuchElementException
    case el :: Nil => el
    case el1 ::  el2 ::  rest => sum((el1 + el2) :: rest)
  }

  def sumOfMax(xs: List[Int]): Int = xs match {
    case Nil => 0
//    case el :: Nil => el
    case el1 ::  el2 ::  rest => sumOfMax((el1 + el2) :: rest)
//    case el1 :: rest => sum(rest) + el1
  }

//  // Applique g à la liste de nombre en paramètres
  def f(list: List[Int], g: Int => Int): List[Int] = ???

  def increment(list: List[Int]): List[Int] = {
    f(list, g => g +1)
  }

  def curry(f: (Int, Int) => Int): Int => Int => Int = {
//     f.curried
    (x: Int) => (y: Int) => f(x, y)
  }

  def uncurry(f: Int => Int => Int): (Int, Int) => Int = {
    (x: Int, y: Int) => f(x)(y)
  }

  /**
   * Ecrire les foncitons f et g qui multiplient respectivement leur paramètre par 3 et 4
   * Ecrire à partir de f et g la fonction h qui multiplie son paramètre par 12.
   */

  val f : Int => Int = (x: Int) => x *3
  val g : Int => Int = (x: Int) => x *4

//  val h: Int = f(g(2));

//   Ecrire la fonction compose qui passe le résultat de son 1er argument à son 2nd:

  def compose[A, B, C](g: A => B, f: B => C): A => C = a => f(g(a))

  //Quel est le résultat des expressions suivantes:

  def f(b: Int): Int = b / 2
  def g(a: Int): Int = a + 2

  (compose(f, g)(0) == compose(g, f)(0)) // == ???

  compose(f, g)(2) // == ???

  compose(g, f)(2) // == ???


  trait A {
    def foo = "A"
  }

  trait B extends A {
    override def foo = "B" + super.foo
  }

  trait C extends A {
    override def foo = "C" + super.foo
  }

  class D
//
////  case class CustomList[A](elems: A){
////
////  }
//
////  case class CustomList[A](elems: A){
////
////  }
//
//  object CustomList {
//    def apply[B](elems: B) = new CustomList(elems)
//  }
//  trait CustomList[+A]
//
//  object Nil extends CustomList[Nothing]
//
//  final case class NonNil[A] (head: A, tail: CustomList[A]) extends CustomList[A]
//
//  object CustomList {
//    def apply() = Nil
//    def apply[A](head: A, tail: CustomList[A]) = CustomList(head, tail)
//  }

  sealed trait Telephone
  case class Smartphone(marque: String, prix: Int, resolution: String,  compatible4g: Boolean) extends Telephone
  case class Fixe(marque: String, prix: Int, nbPiles: Int) extends Telephone
  case class Mobile(marque: String) extends Telephone


  def obtenirMarque(telephone: Telephone): String = telephone match {
    case Smartphone(marque, _, _, _) => marque
    case Fixe(marque, _, _) => marque
    case Mobile(marque) => marque
  }

  case class Article(name: String, price: Int, quantity: Int)

  case class Basket(articles: List[Article]) {
    def display(): Unit = {
      for (article <- articles) {
        ???
      }
    }
  }


  /**
   * Écrivez un programme Scala et utilisez une classe abstraite pour définir un étudiant qui doit avoir un nom de type
   * String et un âge de type Int. Chaque etudiant doit également avoir une méthode printName() avec Unit comme type de retour.
   * Ensuite, créez deux sous-classes de la classe Student, à savoir, un PrimaryStudent et un SecondaryStudent.
   * Chaque sous-classe doit fournir sa propre implémentation de la méthode printName().
   * Vous devez également définir les objets compagnons respectifs pour les types PrimaryStudent et SecondaryStudent.
   * Ensuite, créez les étudiants suivants:
   *    - John, élève du primaire et âgé de 8 ans.
   *    - Jill qui est élève du primaire et a 10 ans.
   *    - James, élève du secondaire et âgé de 13 ans.
   *    - Joe qui est un élève du secondaire et a 14 ans.
   *    - Jack qui est un élève du secondaire et a 11 ans.
   * Ensuite, utilisez une structure de données appropriée pour stocker les étudiants mentionnés ci-dessus.
   * Et créez une liste de noms de méthode listStudents qui prend en entrée cette liste et appelle la méthode printName() pour chacun d’eux.
   */

  sealed abstract class Student(name: String, age: Int) {
    def printName(): Unit
  }

  final case class PrimaryStudent(name: String, age: Int) extends Student(name, age) {
    override def printName(): Unit = {
      println(s"Primary : $name $age")
    }
  }

  final case class SecondaryStudent(name: String, age: Int) extends Student(name, age) {
    override def printName(): Unit = {
      println(s"Secondary : $name $age")
    }
  }

  def listStudents(students: List[Student]): Unit = {
    for (student <- students) {
      student.printName()
    }
  }

  /**
   * Nous voulons dans cet exercice représenter une structure de donnée arborescence qui nous permettra de stocker des entiers.
   * Un arbre ne pourra être que de 2 types:
   *    - une feuille qui contiendra un entier
   *    - un noeud, qui est composé de deux sous-arbres, un à gauche et un à droite du noeud.
   *  Le noeud ne contient pas de valeur.
   */

  sealed abstract class Tree() {
    def isSubTree(tree: Tree): Boolean ={
      if (this == tree)  return true
      this match {
        case Node(left, right) => left.isSubTree(tree) || right.isSubTree(tree)
        case Leaf(_) => false
     }
    }
  }

  final case class Node(left: Tree, right: Tree) extends Tree

  final case class Leaf(value: Int) extends Tree

  def countNode(tree: Tree): Int = tree match {
    case Node(left, right) => 1 + countNode(left) + countNode(right)
    case Leaf(_) => 0
  }

  def maxLength(tree: Tree): Int = tree match {
    case Node(left, right) => {
      val ml = maxLength(left)
      val mr = maxLength(right)
      if (ml > mr) ml +1 else mr + 1
    }
    case Leaf(value) => 0
  }


}
