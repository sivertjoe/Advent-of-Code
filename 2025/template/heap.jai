#import "Basic";

Heap :: struct(Value_Type: Type) {
    buffer: [..] Value_Type;
    count: int;
}

heap_free :: (heap: *Heap) {
    array_free(heap.buffer);
}

heap_push :: (heap: *Heap, item: heap.Value_Type) {
    array_add(*heap.buffer, item);
    shift_up(heap, heap.count);
    heap.count += 1;
}

heap_pop :: (heap: *Heap) -> heap.Value_Type, bool {
    dummy: heap.Value_Type;
    if heap.count == 0 return dummy, false;

    first := heap.buffer[0];

    last := pop(*heap.buffer);
    heap.count -= 1;

    if heap.count > 0 {
        heap.buffer[0] = last;
        shift_down(heap, 0);
    }

    return first, true;
}

shift_up :: (heap: *Heap, i: int) {
    while i > 0 {
        p := (i - 1) / 2;
        if (heap.buffer[i] >= heap.buffer[p]) break;
        heap.buffer[i], heap.buffer[p] = heap.buffer[p], heap.buffer[i];
        i = p;
    }
}

shift_down :: (heap: *Heap, i: int) {
    n := heap.count;
    while true {
        left := 2*i + 1;
        right := 2*i + 2;
        smallest := i;

        if left < n && heap.buffer[left] < heap.buffer[smallest]
            smallest = left;
        if right < n && heap.buffer[right] < heap.buffer[smallest]
            smallest = right;

        if smallest == i break;

         heap.buffer[i], heap.buffer[smallest] = heap.buffer[smallest], heap.buffer[i];
        i = smallest;
    }
}

for_expansion :: (heap: *Heap, body: Code, flags: For_Flags) #expand {
    #assert(!(flags & .REVERSE));

    for * value, idx : heap.buffer {
        #if flags & .POINTER {
             `it := value;
        } else {
            `it := <<value;
        }

        `it_index := idx;
        #insert body;
    }
}
