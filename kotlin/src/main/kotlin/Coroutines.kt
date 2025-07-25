package de.activegroup

import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

/*
public suspend fun <R> coroutineScope(
    block: suspend CoroutineScope.() -> R
): R

erfordert, daß block mittendrin "angehalten" und später forgesetzt.
müssen "Programmstelle" speichern, später "reaktivieren"
=> brauchen "Callbacks"

(alte) JVM: geht nur, indem der Thread blockiert => teuer
 */
suspend fun doWorld() = coroutineScope {  // this: CoroutineScope
    launch {
        delay(1000L)
        println("World!")
    }
    println("Hello")
}

// blockiert
fun getResponse(request: String): Int = TODO()

fun <A> getResponseV2(request: String, continuation: (Int) -> A): A {
    return continuation(TODO());
}

suspend fun bar0(): Int {
    val x1 = getResponse("service1") // flatMap / continuation
    val x2 = getResponse("service2") // flatMap / continuation
    return x1+x2
}

fun bar():Int =
    getResponseV2("service1") { x1 ->
        getResponseV2("service2") { x2 ->
        x1 + x2
    }}

