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

fun defineTable(ctor: Constructor, vararg columns: HeaderWithType): Table {
    val headerrow = Rowdefinition(Direction.HORIZONTAL, columns.map { Header(it.title) },NullConstructor)
    val rowdefinition = Rowdefinition(Direction.HORIZONTAL, columns.map { Cell(it.type)}, ctor)
    return Rowdefinition(Direction.VERTICAL,
        listOf(headerrow,
            Tabledef(Direction.VERTICAL, rowdefinition)),
        object : Constructor {
            override fun apply(vararg args: Any): Any = args[1]
        })
}

val t2 = defineTable(
    cellOutputConstructor,
    HeaderWithType("Segment", Type.STRING),
    HeaderWithType("Country", Type.STRING),
    HeaderWithType("Units Sold", Type.INT),
    HeaderWithType("Manuf. Price", Type.CURRENCY),
    HeaderWithType("Sale Price", Type.CURRENCY),
    HeaderWithType("Sales", Type.CURRENCY),
    HeaderWithType("Profit", Type.CURRENCY),
)

interface T3 {
    fun headerWithType(title: String, type: Type)
}

fun table(ctor: Constructor, block: T3.() -> Unit): Table {
    val columns = mutableListOf<HeaderWithType>()
    val helper = object : T3 {
        override fun headerWithType(title: String, type: Type) {
            columns.add(HeaderWithType(title, type))
        }
    }
    block(helper)
    return defineTable(
        ctor,
        *columns.toTypedArray()
    )
}

val t3 = table(cellOutputConstructor) {
    headerWithType("Segment", Type.STRING)
    headerWithType("Country", Type.STRING)
    headerWithType("Units Sold", Type.INT)
    headerWithType("Manuf. Price", Type.CURRENCY)
    headerWithType("Sale Price", Type.CURRENCY)
    headerWithType("Sales", Type.CURRENCY)
    headerWithType("Profit", Type.CURRENCY)
}