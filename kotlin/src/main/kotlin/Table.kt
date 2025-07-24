package de.activegroup

sealed interface Table

enum class Type {
    STRING, INT, CURRENCY
}

data class Cell(val type: Type): Table
data class Header(val title: String): Table

enum class Direction {
    HORIZONTAL, VERTICAL
}

data class Tabledef(val direction: Direction, val content: Table): Table

sealed interface Constructor

data class Rowdefinition(val direction: Direction,
                         val list: List<Table>,
                         val constructor: Constructor)