package de.activegroup

data class CellOutput(val segment: String, val country: String,
                      val unitsSold: Int, val manufacturingPrice: Currency,
                      val salePrice: Currency,
                      val sales: Currency, val profit: Currency)

val headerrow =
    Rowdefinition(Direction.HORIZONTAL,
        listOf(Header("Segment"),
            Header("Country"),
            Header("Units Sold"),
            Header("Manuf. Price"),
            Header("Sale Price"),
            Header("Sales"),
            Header("Profit")),
        null)

val rowdefinition =
    Rowdefinition(Direction.HORIZONTAL,
        listOf(Cell(Type.STRING), Cell(Type.STRING),
            Cell(Type.INT),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY),
            Cell(Type.CURRENCY)
            ),
        null)

val t = Rowdefinition(Direction.VERTICAL,
    listOf(headerrow, Tabledef(Direction.VERTICAL, rowdefinition)),
    null)