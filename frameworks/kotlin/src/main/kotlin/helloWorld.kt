package codewars

// https://github.com/JetBrains/kotlin-examples/blob/master/gradle/hello-world/src/main/kotlin/demo/helloWorld.kt
fun getGreeting(): String {
    val words = mutableListOf<String>()
    words.add("Hello,")
    words.add("world!")

    return words.joinToString(separator = " ")
}

fun main(args: Array<String>) {
    println(getGreeting())
}
