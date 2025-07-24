package de.activegroup

fun parseTContents(table: Table, tcontents: (x: Int, y: Int) -> Any, x: Int, y: Int): Any {
    return when (table) {
        is Header -> {
            require(tcontents(x,y) == table.title) {"Title mismatch: ${table.title}"}
            table.title
        }
        is Cell -> {
            when (table.type) {
                Type.STRING -> {
                    val content = tcontents(x,y)
                    if (content is  String) {
                        return content
                    } else {
                        throw Error("Expected String")
                    }
                }
                Type.INT -> {
                    val content = tcontents(x,y)
                    if (content is Int) {
                        return content
                    } else {
                        throw Error("Expected Int")
                    }
                }
                Type.CURRENCY -> {
                    val content = tcontents(x,y)
                    if (content is Currency) {
                        return content
                    } else {
                        throw Error("Expected Currency")
                    }
                }
            }
        }
        is Rowdefinition -> {
            var newX = x
            var newY = y
            val output: MutableList<Any> = mutableListOf()

            for (i in 0..table.list.size - 1 ) {
                val el = table.list[i]
                output.add(parseTContents(el, tcontents, newX, newY))

                if (i < table.list.size - 1) {
                    when (table.direction) {
                        Direction.VERTICAL -> newY = newY + el.height()
                        Direction.HORIZONTAL -> newX = newX + el.width()
                    }
                }

            }

            table.constructor.apply(*output.toTypedArray())
        }
        is Tabledef -> {
            var newX = x
            var newY = y
            val output: MutableList<Any> = mutableListOf()

            try {
                while (true) {
                    output.add(parseTContents(table.content, tcontents, newX, newY))
                    when (table.direction) {
                        Direction.VERTICAL -> newY = newY + table.content.height()
                        Direction.HORIZONTAL -> newX = newX + table.content.width()
                    }
                }

            } catch (e: CoordinatesOutOfBounds) {

            }

            output.toList()
        }
    }
}
