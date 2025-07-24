package de.activegroup

data class CellOutput(val segment: String, val country: String,
                      val unitsSold: Int, val manufacturingPrice: Currency,
                      val salePrice: Currency,
                      val sales: Currency, val profit: Currency)

val headerrow =
    Rowdefinition(Direction.HORIZONTAL,
        listOf())