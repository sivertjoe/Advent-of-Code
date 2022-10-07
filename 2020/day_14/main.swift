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
            // list.append(Ins.mask(String(sub)))
        }
        else
        {
           //  var first = line.firstIndex(of: "[")!
           //  first = line.index(after: first)
           //  
           //  var second = line.firstIndex(of: "]")!
           //  second = line.index(before: second)
           //
           //  let idx = line[first...second]
           //  
           // 
           //  var numidx = line.lastIndex(of: " ")!
           //  numidx = line.index(after: numidx)
           //  let num = line[numidx...]
           //  list.append( Ins.addr(idx: Int(idx)!, val: Int(num)!))
        }
    }
    return list
}
   
let list = readInput()
