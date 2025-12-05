#import "Basic";

Queue :: struct(Value_Type: Type) {
    data: []Value_Type;
    head: int;
    tail: int;
    capacity: int;
}

queue_init :: (q: *Queue)  {
    q.capacity = 8;
    q.data = NewArray(q.capacity, q.Value_Type);
}

_queue_grow :: (q: *Queue) {
    new_capacity := q.capacity * 2;
    new_data := NewArray(new_capacity, q.Value_Type);

    n := q.data.count;
    for i : 0..n-1 {
        new_data[i] = q.data[(q.head + i) % q.capacity];
    }

    old := q.data;
    defer array_free(old);

    q.data = new_data;
    q.capacity = new_capacity;
    q.head = 0;
    q.tail = n;
}

queue_push :: (q: *Queue, item: q.Value_Type) {
    next_tail := (q.tail + 1) % q.capacity;

    if next_tail == q.head {
        _queue_grow(q);
        next_tail = (q.tail + 1) % q.capacity;
    }

    q.data[q.tail] = item;
    q.tail = next_tail;
}

queue_pop :: (q: *Queue) -> q.Value_Type {
    assert(q.head != q.tail);

    value := q.data[q.head];
    q.head = (q.head + 1) % q.capacity;
    return value;
}

queue_peek :: (q: *Queue) -> q.Value_Type {
    assert(q.head != q.tail);

    value := q.data[q.head];
    return value;
}

queue_destroy :: (q: *Queue) {
    array_free(q.data);
}
