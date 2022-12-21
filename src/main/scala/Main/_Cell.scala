package Main

object _Cell {

  sealed abstract class Cell {

    def padded(maxPad: Int): String

    def getOptions: List[Int]

    def isEmpty: Boolean
    def includes(num: Int): Boolean
    def without(num: Int): Cell

    def length: Int

    def getValue: Int

    def stuck: Boolean = false

    def includesSome(vls: List[Int]): Boolean

    def withoutAll(vls: List[Int]): Cell

    def intersect(vls: List[Int]): Cell

    def hasBeyond(vls: List[Int]): Boolean =
      this.getOptions.exists(x => ! vls.contains(x))

    def includes(cell: Cell): Boolean

    def isFull: Boolean = !isEmpty

  }

  case class Empty(options: List[Int]) extends Cell {
    override def isEmpty: Boolean = true

    override def includes(num: Int): Boolean = options.contains(num)

    override def without(num: Int): Empty = Empty(options.filter(_ != num))

    override def getValue: Int = throw new Error("Empty does not have value")

    override def toString: String =
      options.mkString("[", ",", "]").padTo(19, ' ')

    override def stuck: Boolean = options.isEmpty

    override def padded(maxPad: Int): String = options.mkString("[", ",", "]").padTo(maxPad, ' ')

    override def length: Int = options.length

    override def includesSome(vls: List[Int]): Boolean = options.intersect(vls).nonEmpty

    override def withoutAll(vls: List[Int]): Cell =
      Empty(options.filter(x => !vls.contains(x)))

    override def getOptions: List[Int] = options

    override def intersect(vls: List[Int]): Cell = Empty(options.intersect(vls))

    override def includes(cell: Cell): Boolean =
      cell match {
        case Empty(other_options) => other_options.forall(opt => this.options.contains(opt))

        case Full(_, _) => false
      }
  }

  case class Full(num: Int, filled: Boolean = true) extends Cell {
    assert(1 <= num && num <= 9)

    override def isEmpty: Boolean = false

    override def includes(num: Int): Boolean = false

    override def without(num: Int): Cell = this

    override def getValue: Int = num

    override def toString: String =
      " ".repeat(9) + num.toString + " ".repeat(9)

    override def padded(maxPad: Int): String = num.toString + " ".repeat(maxPad - 1)

    override def length: Int = -1

    override def includesSome(vls: List[Int]): Boolean = false

    override def withoutAll(vls: List[Int]): Cell = this

    override def getOptions: List[Int] = Nil

    override def intersect(vls: List[Int]): Cell = this

    override def includes(cell: Cell): Boolean = false
  }

  def makeEmpty: Empty = Empty(List.range(1,  10))

}