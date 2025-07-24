package de.activegroup

sealed interface Table {
    fun width(): Int = when (this) {
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

    fun height(): Int = when (this) {
            is Cell -> 1
            is Header -> 1
            is Rowdefinition ->
                when (this.direction) {
                    Direction.VERTICAL -> this.list.sumOf { it.height() }
                    Direction.HORIZONTAL -> this.list.maxOf { it.height() }
                }
            is Tabledef ->
                when (this.direction) {
                    Direction.VERTICAL -> throw Exception("height of a horizontal Tabledef is not defined")
                    Direction.HORIZONTAL -> this.content.height()
                }
        }
}

class CoordinatesOutOfBounds(val x: Int, val y: Int): RuntimeException() {}

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

interface Constructor {
    fun apply(vararg varargs: Any): Any
}

data class Rowdefinition(val direction: Direction,
                         val list: List<Table>,
                         val constructor: Constructor): Table