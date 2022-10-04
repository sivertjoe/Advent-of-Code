using DelimitedFiles
using Printf

function read_input(filename)
    lines = String[]
    open("input", "r") do f
        for line in eachline(f)
            push!(lines, line)
        end
    end
    
    [collect(line) for line in lines]
end

function calc_point(c, vec, i, j, num, p2)
    if c == 'L' && adjacent(vec, i, j, p2) == 0
        '#'
    elseif c == '#' && adjacent(vec, i, j, p2) >= num
        'L'
    else
        c
    end

end

function calc_next_step(vec, num, p2)
    next = deepcopy(vec)

    for i in 1:length(vec)
        for j in 1:length(vec[1])
            next[i][j] = calc_point(vec[i][j], vec, i, j, num, p2)
        end
    end
    next
end

function count_occupied(vec)
    flat = collect(Iterators.flatten(vec))
    count(x -> x == '#', flat)
end


function in_range(vec, i, j)
    width = length(vec[1])
    height = length(vec)
    i >= 1 && i <= height && j >= 1 && j <= width
end


function find_empty_seat(vec, px, x, py, y)
    while in_range(vec, px, py) && vec[px][py] == '.'
        px += x
        py += y
    end
    px, py
end

function adjacent(vec, i, j, p2=nothing)
    sum = 0
    
    width = length(vec[1])
    height = length(vec)
    
    for x in -1:1
        for y in -1:1
            if x == 0 && y == 0
                continue
            end
            
            px = i + x
            py = j + y
            
            if p2 != nothing
                px, py = p2(vec, px, x, py, y)
            end
            
            if in_range(vec, px, py)
                sum += vec[px][py] == '#'
            end
        end
    end
    sum
end

function calc_seats(vec, num, p2=nothing)
    while true
        next = calc_next_step(vec, num, p2)

        if next == vec
            return count_occupied(next)
        end
        vec = next
    end
end

function time(f, vec)
    t0 = time_ns()
    res = f(vec)
    t1 = time_ns()
    diff = (t1 - t0) / 1000000
    @printf("(%ims)\t%i\n", diff, res)
end


function part_one(vec)
    calc_seats(vec, 4)
end

function part_two(vec)
    calc_seats(vec, 5, find_empty_seat)
end


input = read_input("input")
time(part_one, input)
time(part_two, input)
