require 'set'

$movs = {
  'ne' => [1, 0, -1],
  'e' => [1, -1, 0],
  'se' => [0, -1, 1],
  'nw' => [0, 1, -1],
  'w' => [-1, 1, 0],
  'sw' => [-1, 0, 1],
}

def add(hex, mov)
  h = hex.dup
  h[0] += mov[0]
  h[1] += mov[1]
  h[2] += mov[2]

  h
end

def parseLine(line)
  hex = [0, 0, 0]
  line.each { |dir|
    hex = add(hex, $movs[dir])
  }

  hex
end

def createSet(input)
  set = Set.new
  input.each { |line| 
    set ^= [parseLine(line)]
  }

  set
end

def partOne(input)
  createSet(input).length
end

def partTwo(input)
  set = createSet(input)

  for _ in 1..100 do
    all = set.dup
    nextSet = set.dup
    set.each { |hex| $movs.each_value { |mov| all << add(hex, mov) } }

    all.each { |hex|
        count = $movs.each_value.count {|mov| set === add(hex, mov) }

        if set === hex && (count == 0 || count > 2)
            nextSet.delete(hex)
        elsif !(set === hex) && count == 2
            nextSet.add(hex)
        end
    }
    set = nextSet
  end

  set.length
end

input = File.open("input").readlines.map { |l| l.scan(/e|se|sw|w|nw|ne/) }
puts partOne input
puts partTwo input
