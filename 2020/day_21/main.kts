import java.io.File

data class Food(val Ingredients: Set<String>, val Allergens: Set<String>)

fun readInput(filename: String): List<String>
    = File(filename).readLines()

fun main() {
    val input = readInput("input")
    println(partOne(input))
    println(partTwo(input))
}

main()

fun parseLine(line: String): Food {
    val sub = line.split(" (")
    val ing = HashSet(sub[0].split(' '))
    val all = HashSet(sub[1].substring(sub[1].indexOf(' ') + 1, sub[1].indexOf(')')).split(", "))

    return Food(ing, all)
}

fun parse(input: List<String>): List<Food> = input.map({ parseLine(it) })

fun getAllergens(input: List<String>): Pair<MutableList<String>, HashMap<String, Set<String>>> {
    val foods = parse(input)
    var ingredients = mutableListOf<String>()
    var allergens = HashMap<String, Set<String>>()

    for (food in foods) {
        ingredients.addAll(food.Ingredients)
        for (alg in food.Allergens) {
            if (allergens.get(alg) == null) {
                allergens.put(alg, food.Ingredients.toSet())
            } else {
                var s = allergens.getValue(alg)
                s = s.intersect(food.Ingredients.toSet())
                allergens.put(alg, s)
            }
        }
    }
    return Pair(ingredients, allergens)
}

fun removeEntries(map: HashMap<String, HashSet<String>>, p: Pair<String, String>) {
    map.remove(p.first)
    for (value in map.values) {
        value.remove(p.second)
    }
}

fun findPair(map: HashMap<String, HashSet<String>>): Pair<String, String> {
    val (key, value) = map.entries.find({ it.value.count() == 1 })!!
    return Pair(key, value.elementAt(0))
}

fun partOne(input: List<String>): Int {
    val (ingredients, allergens) = getAllergens(input)
    val allergen_foods = allergens.values.flatten().toSet()
    return ingredients.filter({ !allergen_foods.contains(it) }).count()
}

fun partTwo(input: List<String>): String {
    var (_, allergens) = getAllergens(input)
    var map = HashMap<String, HashSet<String>>()
    for ((key, value) in allergens) {
        map.put(key, value.toHashSet())
    }
    var res = mutableListOf<Pair<String, String>>()

    while (map.count() > 0) {
        val p = findPair(map)
        removeEntries(map, p)
        res.add(p)
    }

    res.sortBy({ it.first })
    return res.map({ it.second }).joinToString(",")
}
