
//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  //TIP Press <shortcut actionId="ShowIntentionActions"/> with your caret at the highlighted text
  // to see how IntelliJ IDEA suggests fixing it.
  (1 to 5).map(println)

  for (i <- 1 to 5) do
    //TIP Press <shortcut actionId="Debug"/> to start debugging your code. We have set one <icon src="AllIcons.Debugger.Db_set_breakpoint"/> breakpoint
    // for you, but you can always add more by pressing <shortcut actionId="ToggleLineBreakpoint"/>.
    println(s"i = $i")
    
def optionAdd(oi1: Option[Int], oi2: Option[Int]): Option[Int] =
  for {
    i1 <- oi1
    i2 <- oi2
  } yield i1+i2

  /*{
  oi1.flatMap { i1 =>
    oi2.flatMap { i2 =>
      Some(i1+i2)
    }
  }
}  */

