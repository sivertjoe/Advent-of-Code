def read_input() -> list[int]:
    return [int(line) for line in open("input", "r").readlines()]

def handshake(subject_number: int, loop_size: int) -> int:
    res = 1
    for _ in range(loop_size):
        res *= subject_number
        res %= 20201227
    return res

def loop_size(subject_number: int, public_key: int) -> int:
    res = 1
    i = 1
    while True:
        res *= subject_number
        res %= 20201227
        if res == public_key:
            return i
        i += 1

def loop_sizes(input: list[int]) -> tuple[int, int]:
    return (loop_size(7, input[0]), loop_size(7, input[1]))

def part_one(input: list[int]) -> int:
    ls = loop_sizes(input)
    return handshake(input[0], ls[1])

if __name__ == "__main__":
    input = read_input()
    print(part_one(input))
