enum Ins
{
    case mask(String)
    case addr(idx: Int, val: Int)
}

func readInput() -> [Ins]
{
    var list: [Ins] = []
    while let line = readLine()
    {
        if line.hasPrefix("mask")
        {
            let mask = line.lastIndex(of: " ")!
            let sub = line[mask...]
            list.append(Ins.mask(String(sub)))
        }
        else
        {
            var first = line.firstIndex(of: "[")!
            first = line.index(after: first)
            
            var second = line.firstIndex(of: "]")!
            second = line.index(before: second)

            let idx = line[first...second]
            
           
            var numidx = line.lastIndex(of: " ")!
            numidx = line.index(after: numidx)
            let num = line[numidx...]
            list.append( Ins.addr(idx: Int(idx)!, val: Int(num)!))
        }
    }
    return list
}


func setBit(value: Int, bit: Bool, pos: Int) -> Int
{
    if (!bit) 
    {
        return value & ~(1 << pos) 
    }
    else
    {
        return value | 1 << pos 
    }
                    
}

func getNum(_ num: Int, _ mask: String) -> Int
{
    var res = num;
    for (i, c) in mask.reversed().enumerated()
    {
        if (c != "X")
        {
            res = setBit(value: res, bit: c == "1", pos: i)
        }
    }
    return res
}

func partOne(_ list: [Ins]) -> Int
{
    var memory = [Int : Int]()
    
    var mask = ""
    for ins in list
    {
        switch ins
        {
            case .addr(idx: let idx, val: let val):
                memory[idx] = getNum(val, mask)
            case .mask(mask: let newMask):
                mask = newMask
        }
    }
    
    return memory.reduce(0, { acc, pair in
        acc + pair.1        
    })
}

// Applies the mask and extracts the Xs' index values
func helper(_ addr: Int, _ mask: String) -> (Int, [Int])
{
    var addr = addr;
    var list: [Int] = [];
    
    for (i, c) in mask.reversed().enumerated()
    {
        if (c == "X")
        {
            list.append(i)
        }
        else if c == "1"
        {
            addr |= 1 << i;
        }
    }
    
    return (addr, list)
}

func _addrs(_ addr: Int, _ list: [Int], _ i: Int) -> [Int]
{
    if (i == list.count)
    {
        return [addr]
    }
    
    let low = setBit(value: addr, bit: false, pos: list[i])
    let high = setBit(value: addr, bit: true, pos: list[i])
    
    return _addrs(low, list, i + 1) + _addrs(high, list, i + 1)
}

func addrs(_ addr: Int, _ mask: String) -> [Int]
{
    let (addr, list) = helper(addr, mask)
    return _addrs(addr, list, 0)
}

func partTwo(_ list: [Ins]) -> Int
{
    var memory = [Int : Int]()
    
    var mask = ""
    for ins in list
    {
        switch ins
        {
            case .addr(idx: let idx, val: let val):
                for addr in addrs(idx, mask)
                {
                    memory[addr] = val
                }
            case .mask(mask: let newMask):
                mask = newMask
        }
    }
    
    return memory.reduce(0, { acc, pair in
        acc + pair.1        
    })
}
   
let list = readInput()
print(partOne(list))
print(partTwo(list))