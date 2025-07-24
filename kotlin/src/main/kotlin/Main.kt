package de.activegroup

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
fun main() {
    println("Running application...")

    val exampleList = listOf(
        listOf("Segment","Country","Units Sold","Manuf. Price","Sale Price","Sales","Profit"),
        listOf("Government","Canada", 1618, "$3,00","$20,00","$32.370,00","$16.185,00"),
        listOf("Government","Germany", 1321, "$3,00","$20,00","$26.420,00","$13.210,00"),
        listOf("Midmarket","France", 2178, "$3,00","$15,00","$32.670,00","$10.890,00"),
        listOf("Midmarket","Germany", 888, "$3,00","$15,00","$13.320,00","$4.440,00"),
        listOf("Midmarket","Mexico", 2470, "$3,00","$15,00","$37.050,00","$12.350,00")
    )

    fun tcontents(x: Int, y: Int): Any = try {
            exampleList[y][x]
        } catch (e: Exception) {
            throw CoordinatesOutOfBounds(x, y)
        }

    println(parseTContents(t, ::tcontents, 0, 0))
}