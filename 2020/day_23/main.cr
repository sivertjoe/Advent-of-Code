class Node
    property num : Int32 
    property nextNode

    def initialize(num : Int32)
        @num = num
        @nextNode = uninitialized Node
    end

    def nextN(n : Int32)
        temp = @nextNode
        i = 0
        while i < n - 1
            temp = temp.nextNode
            i += 1
        end
        temp
    end
        
end

def getOutput(head, n) : String
    while true
        if head.num == 1
            head = head.nextNode
            break
        end
        head = head.nextNode
    end

    array = Array.new(n, 0)
    i = 0
    while i < n
        array[i] = head.num
        head = head.nextNode
       i += 1 
    end
    array.join ""
end

def threeContains(n, three) : Bool
    i = 0
    while i < 3
        if three.num == n
            return true
        end
        three = three.nextNode
        i += 1
    end
    return false
end

def destCup(size, curr, hash, three) : Node
    n = curr.num
    while true
        n -= 1
        if n == 0
            n = size
        end

        if !threeContains(n, three)
            return hash[n]
        end
    end
end

def take3(curr) : Node
    first = curr.nextNode

    # snippity snap
    curr.nextNode = curr.nextN 4

    first
end

def moveThree(dest, three)
    nextNodes = dest.nextNode
    dest.nextNode = three
    three.nextN(2).nextNode = nextNodes
end


def simulate(numbers, n) : Node
    hash = Hash(Int32, Node).new

    head = Node.new numbers[0]
    hash[numbers[0]] = head
    temp = head

    i = 1
    while i < numbers.size
        newNode = Node.new numbers[i]
        hash[numbers[i]] = newNode
        temp.nextNode = newNode
        temp = newNode
        i += 1
    end
    temp.nextNode = head

    i = 0
    while i < n
        curr = head
        three = take3(curr)
        dest = destCup(numbers.size, curr, hash, three)
        moveThree(dest, three)
        head = head.nextNode
        i += 1
    end
    head
end

def partOne(numbers) : String
    getOutput(simulate(numbers, 100), 8)
end

def partTwo(initials) : UInt64
    numbers = Array.new(1_000_000, 0)
    i = 0

    while i < initials.size
        numbers[i] = initials[i]
        i += 1
    end

    while i < 1_000_000
        numbers[i] = i + 1
        i += 1
    end

    head = simulate(numbers, 10_000_000)

    while true
        if head.num == 1
            head = head.nextNode
            break
        end
        head = head.nextNode
    end

    head.num.to_u64 * head.nextNode.num.to_u64
end

numbers = File.read("input").chomp.chars.map &.to_i.not_nil!
puts partOne numbers
puts partTwo numbers
