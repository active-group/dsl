package de.activegroup

// in der Realität: Vavr
sealed interface Option<out A> {
    fun getUnsafe(): A
    suspend fun susp(): A = MonadDSL.susp<Option<A>, A>(this::flatMap)

    fun <B> map(f: (A) -> B): Option<B> =
        when (this) {
            is None -> None
            is Some -> Some(f(this.value))
        }

    fun <B> flatMap(f: (A) -> Option<B>): Option<B> =
        when (this) {
            is None -> None
            is Some ->f(this.value)
        }
    companion object {
        fun <A> optionally(block: suspend OptionDSL.() -> A): Option<A> =
            MonadDSL.effect(OptionDSL, block)
    }
}

data object None: Option<Nothing> {
    override fun getUnsafe(): Nothing =
        throw Exception("wanted Some, got None")
}

data class Some<A>(val value: A): Option<A> {
    override fun getUnsafe(): A = value
}

// besser noch wäre: applikativer Funktor -> FUNAR

fun addOptions(oi1: Option<Int>, oi2: Option<Int>): Option<Int> =
    oi1.flatMap { i1 ->
        oi2.flatMap { i2 ->
            Some(i1 + i2)
        }
    }

