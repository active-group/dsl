package de.activegroup

sealed interface Table {
    fun width(): Int =
        when (this) {
            is Cell -> 1
            is Header -> 1
            is Rowdefinition ->
             when (this.direction) {
                Direction.HORIZONTAL -> this.list.sumOf { it.width() }
                 Direction.VERTICAL -> this.list.maxOf { it.width() }
            }
            is Tabledef ->
                when (this.direction) {
                    Direction.HORIZONTAL -> throw Exception("width of a horizontal Tabledef is not defined")
                    Direction.VERTICAL -> this.content.width()
                }
        }

//    val height: Int =
//        when (this) {
//            is Cell -> 1
//            is Header -> 1
//            is Rowdefinition ->
//                when (this.direction) {
//                    Direction.HORIZONTAL -> this.list.sumOf { it.width() }
//                    Direction.VERTICAL -> this.list.maxOf { it.width() }
//                }
//            is Tabledef ->
//                when (this.direction) {
//                    Direction.HORIZONTAL -> throw Exception("width of a horizontal Tabledef is not defined")
//                    Direction.VERTICAL -> this.content.width()
//                }
//        }
}

enum class Type {
    STRING, INT, CURRENCY
}

typealias Currency = String

data class Cell(val type: Type): Table
data class Header(val title: String): Table

enum class Direction {
    HORIZONTAL, VERTICAL
}

data class Tabledef(val direction: Direction, val content: Table): Table

sealed interface Constructor {
    fun apply(vararg varargs: Any): Any
}

data class Rowdefinition(val direction: Direction,
                         val list: List<Table>,
                         val constructor: Constructor): Table