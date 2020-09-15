class Parser(l: Lex) {
  val lex: Lex = l
  val vals = new Array[Boolean](25)
  
  def parse(str:String): Boolean = {
    lex.setLine(str)
    val a = S()
    lex.reset()
    a
  }
  
  def S(): Boolean = {
    val r = Sp()
    if (lex.matchChar(Lex.EOL) == 0)
      throw new ParseError("Expected EOL")
    r
  }
  
  def Sp(): Boolean = {
    val a = lex.matchChar('L')
    if (a == 0)
      throw new ParseError("Expected id")
    val r =
      if (lex.matchChar('?') != 0) {
        Query(a)
      }
      else if (lex.matchChar('=') != 0) {
        Assign(a)
      }
      else throw new ParseError("Expected query or assignment operator")
    r
  }
  
  def Query(a: Char): Boolean = {
    println(vals(toId(a)))
    vals(toId(a))
  }
  
  def Assign(id: Char): Boolean = {
    vals(toId(id)) = Exp()
    vals(toId(id))
  }
  
  def Exp(): Boolean = {
    val r = A()
    r & Expp()
  }
  
  def Expp(): Boolean = {
    if(lex.matchChar('&')!=0)
      A() & Expp()
    else
      true //preserves value when ANDed
  }
  
  def A(): Boolean = {
    val r = B()
    r | Ap()
  }
  
  def Ap(): Boolean = {
    if(lex.matchChar('|')!=0)
      B() | Ap()
    else
      false //Doesn't change value when ORed
  }
  
  def B(): Boolean = {
    val r = C()
    r ^ Bp()
  }
  
  def Bp(): Boolean = {
    if(lex.matchChar('^')!= 0)
     C() ^ Bp()
    else
      false //Same as above
  }
  
  def C(): Boolean = {
    if(lex.matchChar('~')!= 0)
      !D()
    else
      D()
  }
  
  @throws(classOf[ParseError])
  def D(): Boolean = {
    if(lex.matchChar('(')!= 0) {
      val a = Exp()
      if(lex.matchChar(')')== 0)
        throw new ParseError("Expected )")
      a
    }
    else
      E()
  }
  def E(): Boolean = {
    if(lex.check('L')!= 0)
      vals(toId(lex.matchChar('L')))
    else if ((lex.check('1')!= 0) || (lex.check('0')!= 0))
      F()
    else
      throw new ParseError("Expected expression")
  }
  
  def F(): Boolean = {
    if(lex.matchChar('1')!=0)
      true
    else if(lex.matchChar('0')!= 0)
      false
    else
      throw new ParseError("Expected literal")
    
  }
  def toId(id: Char): Int = {
    id - 97
  }
}
