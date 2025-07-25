package de.activegroup

// in der Realität: Vavr
sealed interface Option<A>

data object None: Option<Nothing>
data class Some<A>(val value: A): Option<A>



