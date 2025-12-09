#import "Basic";
#import "Hash_Table";
Hash :: #import "Hash";

Set :: struct(
    Value_Type: Type,
    given_hash_function: (t: Value_Type) -> u32 = null,
    given_cmp_function: (a: Value_Type, b: Value_Type) -> bool = null
) {

    #if given_hash_function {
        hash_function :: given_hash_function;
    } else {
        hash_function :: x => Hash.get_hash(x);
    }

    #if given_cmp_function {
        cmp_function :: given_cmp_function;
    } else {
        cmp_function :: (a: Value_Type, b: Value_Type) -> bool { return a == b; };
    }

    table: Table(Value_Type, void, hash_function, cmp_function);
    count: int;
}

set_free :: (set: *Set) {
    deinit(*set.table);
}

set_clone :: (set: *Set($T)) -> Set(T) {
    new: Set(T);
    for set {
        set_add(*new, it);
    }
    return new;
}

set_add :: (set: *Set, item: set.Value_Type) -> bool {
    _, new := find_or_add(*set.table, item);
    if new {
        set.count += 1;
    }
    return new;
}

set_remove :: (set: *Set, item: set.Value_Type) -> bool {
    contains := table_remove(*set.table, item);
    if contains {
        set.count -= 1;
    }
    return contains;
}

set_contains :: (set: *Set, item: set.Value_Type) -> bool {
    return table_contains(*set.table, item);
}

for_expansion :: (set: *Set, body: Code, flags: For_Flags) #expand {
    #assert(!(flags & .REVERSE));

    for * value, key : set.table {
        #if flags & .POINTER {
             `it := *key;
        } else {
            `it := key;
        }

        `it_index := null;
        #insert body;
    }
}

set_union :: (set1: *Set, set2: *Set) -> Set(set1.Value_Type, set1.hash_function, set1.cmp_function) {
    assert(set1.Value_Type == set2.Value_Type);
    new: Set(
        set1.Value_Type,
        set1.hash_function,
        set1.cmp_function
    );

    for set1 {
        set_add(*new, it);
    }
    for set2 {
        set_add(*new, it);
    }
    return new;
}

set_intersection :: (set1: *Set($T), set2: *Set(T)) -> Set(T) {
    new := set_union(set1, set2);

    to_remove: [..]T;
    defer array_free(to_remove);
    for new {
        if !set_contains(set1, it) || !set_contains(set2, it) {
            array_add(*to_remove, it);
        }
    }

    for to_remove {
        set_remove(*new, it);
    }

    return new;
}

set_difference :: (set1: *Set($T), set2: *Set(T)) -> Set(T) {
    empty: Set(T);
    new := set_union(set1, *empty);

    for set2 {
        set_remove(*new, it);
    }

    return new;
}

//
// main :: () {
//     set1: Set(int);
//     defer set_free(*set1);
//     set2: Set(int);
//     defer set_free(*set2);
//
//     for 0..2 set_add(*set1, it);
//     for 3..5 set_add(*set2, it);
//
//     set3 := set_union(*set1, *set2);
//     defer set_free(*set3);
//     print("UNION\n");
//     for set3 {
//         print("%\n", it);
//     }
//
//     set4 := set_intersection(*set3, *set1);
//     defer set_free(*set4);
//
//     print("INTERSECTION\n");
//     for set4 {
//         print("%\n", it);
//     }
//
//     set5 := set_difference(*set3, *set1);
//     defer set_free(*set5);
//
//     print("difference\n");
//     for set5 {
//         print("%\n", it);
//     }
//
//     print("clone\n");
//     set6 := set_clone(*set5);
//     defer set_free(*set6);
//     for set6 {
//         print("%\n", it);
//     }
//
// }
