package de.activegroup

// in der Realität: Vavr
sealed interface Option<A> {
    fun getUnsafe(): A
}

data object None: Option<Nothing> {
    override fun getUnsafe(): Nothing =
        throw Exception("wanted Some, got None")
}

data class Some<A>(val value: A): Option<A> {
    override fun getUnsafe(): A = value
}



