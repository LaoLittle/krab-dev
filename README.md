# Krab-lang

## Currently under heavily development 🚧

Krab - pronounced *Crab* - represents Kotlin and Crab🦀

The preview version of the compiler is under `krab-codegen`.
Use `cargo r` to run it.

An overview of the syntax:

```kotlin
fun fib(n: UInt64): UInt64 {
    if (n <= 1) { return n }

    return fib(n - 1) + fib(n - 2)
}

fun test_aa(n: Int32) {
    if (n > 0) { return }
}

fun test(a: UInt32): Boolean {
    val ret = if (a <= 0) true else false
    return ret
}

fun abc(a: UInt64): Int64 {
    val fuck = if (a >= 2) 1 else -2

    return fuck
}

fun calc(input: UInt32): UInt32 {
    var my_little_language: UInt32 = 0

    while (my_little_language <= 114) {
        my_little_language += 2;
    }

    return my_little_language
}

fun unsafe() {
    val safe = unsafe // function pointer
    safe()
}

fun loop() {
    while (true) {}
}

fun bbc(a: Int32) {
    val d = bbc
    d(114)

    val dada = test
    dada(0)
}

fun test_bool(b: Boolean): Int32 {
    return 114;
}

fun main() {
    while (false) {}

    val a = 114
    val b = 2

    abc((a + 514) * 32 + 114 * 514)


    // bbc(a) error: type mismatched
    // bbc(a+b) error: type mismatched
    bbc(b) // we are good here

    val dd = false && true

    test_bool(dd || true)
}

fun lowering() {
    val a = 1 // this is an abstract int, the type is undefined here.

    val b: UInt32 = a // the type is refined UInt32, abstract int would be lowered to the actual type.

    // val c: Int32 = a // compile error.
}

fun rem_test(i: Int32): Int32 {
    var i2 = i + 1
    i2 %= 4
    return i % 3 + i2
}

fun rem_u_test(i: UInt32): UInt32 {
    var i2 = i + 1
    i2 %= 4
    return i % 3 + i2
}

fun div_test(i: Int32): Int32 {
    var i2 = i + 3
    i2 /= 114
    return i2 + i / 4
}

fun div_u_test(i: UInt32): UInt32 {
    var i2 = i + 3
    i2 /= 3
    return i2 + i / 4
}

fun div_any(i: UInt32, d: UInt32): UInt32 {
    return i / d
}
```