package de.activegroup

import kotlin.coroutines.AbstractCoroutineContextElement
import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.resume
import kotlin.coroutines.startCoroutine
import kotlin.coroutines.suspendCoroutine

/**
 * This class provides some helpers for creating monadic
 * DSLs in Kotlin.
 *
 * It has three parts that work together:
 *
 * - The [effect] method takes a lambda block as an argument that
 *   can contain monadic DSL code.
 * - The [pure] helps implement the monadic unit of the DSL.
 * - The [susp] method helps implement the monadic operations
 *   the DSL.
 *
 * To create a monadic DSL, do the following:
 * - Create a datatype representing the monad, let's call it `F<A>`.
 * - Implement a `Pure` function or constructor, implementing
 *   monadic unit.
 * - Implement a `bind` method with a signature like so:
 * ```
 * fun <B> bind(next: (A) -> F<B>): F<B>
 * ```
 * - Add a `susp` method like so:
 * ```
 * suspend fun susp(): A = MonadDSL.susp<F<A>, A>(this::bind)
 * ```
 * - Create a DSL object `FDSL`, that should have at least this
 *   function:
 * ```
 * suspend fun  <A> pure(result: A): A = FDSL.pure(result) { Pure(it) }
 * ```
 * - Add more monadic functions to `FDSL` as needed, typically
 *   looking like this, where `C` is the constructor of the
 *   monadic operation:
 * ```
 * suspend fun f(<args>) = F.C(args) { Pure(it) }.susp()
 * ```
 *   (See examples in the tests.)
 * - Add a companion object with a function `fcode` delimiting a DSL
 *   code block like so:
 * ```
 * fun <A> fcode(block: suspend FDSL.() -> A): Option<A> =
 *             MonadDSL.effect(FDSL, block)
 * ```
 */
object MonadDSL {

    private class ContextElement<FA>(var contents: FA?) : AbstractCoroutineContextElement(ContextElement) {
        companion object Key : CoroutineContext.Key<ContextElement<*>>
    }

    /**
     * Helper function that runs a monadic code block.
     * See class documentation for usage
     *
     * @param DSL is the DSL class containing the DSL ops.
     * @param FA is `F<A>`, where F is the type of the monad.
     * @param A is the type parameter for the monad.
     */
    fun <DSL, FA, A> effect(dsl: DSL, block: suspend DSL.() -> A, coroutineContext: CoroutineContext = EmptyCoroutineContext): FA {
        val element = ContextElement<FA>(null)
        suspend { dsl.block() }.startCoroutine(
            Continuation(coroutineContext + element) { result ->
                result.onFailure { exception ->
                    val currentThread = Thread.currentThread()
                    currentThread.uncaughtExceptionHandler.uncaughtException(currentThread, exception)
                }
            }
        )
        return element.contents!!
    }

    /**
     * Helper function for implementing monadic unit in the DSL.
     *
     * @param FA is `F<A>` where `F` is the type of the monad.
     * @param A is the result type of the monadic operation.
     * @param result is the argument to the unit.
     * @param pure is the unit operation of the underlying monad.
     */
    @Suppress("UNCHECKED_CAST")
    suspend fun <FA, A> pure(result: A, pure: (A) -> FA): A =
        suspendCoroutine {
            val element = it.context[ContextElement]!! as ContextElement<FA>
            element.contents = pure(result)
            it.resume(result)
        }

    /**
     * Helper function for implementing monadic operations in the DSL.
     *
     * @param FA is `F<A>` where `F` is the type of the monad.
     * @param A is the result type of the monadic operation.
     * @param bind is the `bind` operation of the monad.
     */
    @Suppress("UNCHECKED_CAST")
    suspend fun <FA, A> susp(bind: ((A) -> FA) -> FA): A =
        suspendCoroutine {
            val element = it.context[ContextElement]!! as ContextElement<FA>
            element.contents =
                bind { result ->
                    it.resume(result)
                    element.contents!!
                }
        }
}
