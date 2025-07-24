package de.activegroup

fun parseTContents(table: Table, tcontents: (x: Int, y: Int) -> Any, x: Int, y: Int): Any {
    println("Test")
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
            var output: MutableList<Any> = mutableListOf()
            for (el in table.list) {
                output.add(parseTContents(el, tcontents, newX, newY))

            }

            table.constructor.apply(*output.toTypedArray())


//            return table.constructor.call(*output.toTypedArray())
        }
        is Tabledef -> {}
    }
}

fun tableWidth()