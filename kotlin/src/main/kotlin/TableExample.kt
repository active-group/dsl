package de.activegroup

data class CellOutput(val segment: String, val country: String,
                      val unitsSold: Int, val manufacturingPrice: Currency,
                      val salePrice: Currency,
                      val sales: Currency, val profit: Currency)

object cellOutputConstructor: Constructor {
    override fun apply(vararg varargs: Any): Any = CellOutput(
        varargs[0] as String,
        varargs[1] as String,
        varargs[2] as Int,
        varargs[3] as Currency,
        varargs[4] as Currency,
        varargs[5] as Currency,
        varargs[6] as Currency
    )
}

object NullConstructor: Constructor {
    override fun apply(vararg args: Any): Any = listOf<Any>()
}

val headerrow =
    Rowdefinition(Direction.HORIZONTAL,
        listOf(Header("Segment"),
            Header("Country"),
            Header("Units Sold"),
            Header("Manuf. Price"),
            Header("Sale Price"),
            Header("Sales"),
            Header("Profit")),
        NullConstructor)

val rowdefinition =
    Rowdefinition(Direction.HORIZONTAL,
        listOf(
            Cell(Type.STRING),
            Cell(Type.STRING),
            Cell(Type.INT),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY)
            ),
        cellOutputConstructor)

val t = Rowdefinition(Direction.VERTICAL,
    listOf(headerrow,
        Tabledef(Direction.VERTICAL, rowdefinition)),
    object : Constructor {
        override fun apply(vararg args: Any): Any = args[1]
    })