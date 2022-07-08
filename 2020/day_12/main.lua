function lines(file)
  local lines = {}
  for line in io.lines("input") do 
    lines[#lines + 1] = {
      dir = string.sub(line, 1, 1),
      num = string.sub(line, 2, string.len(line))
    }
  end
  return lines
end

function part_one(lines)
  local x = 0    
  local y = 0
  local angle = 90 -- ship starts facing east
  
  for _, line in pairs(lines) do
    if line.dir == "N" then
      y = y + line.num
    elseif line.dir == "S" then
      y = y - line.num
    elseif line.dir == "E" then
      x = x + line.num
    elseif line.dir == "W" then
      x = x - line.num
    elseif line.dir == "R" then
        angle = angle + line.num
    elseif line.dir == "L" then
        angle = angle - line.num
    else -- forward
      -- have to do this weird floor scheme since: 
      -- math.cos(math.rad(90)) = 6....e-17
      x = x + math.floor(math.sin(math.rad(angle)) * line.num + 0.5)
      y = y + math.floor(math.cos(math.rad(angle)) * line.num + 0.5)
    end
  end
  
  return math.abs(x) + math.abs(y)
end

function part_two(lines)
  local x = 10
  local y = 1

  local sx = 0    
  local sy = 0

  for _, line in pairs(lines) do
    if line.dir == "N" then
      y = y + line.num
    elseif line.dir == "S" then
      y = y - line.num
    elseif line.dir == "E" then
      x = x + line.num
    elseif line.dir == "W" then
      x = x - line.num
    elseif line.dir == "R" then
        x, y = rotate(x, y, line)
    elseif line.dir == "L" then
        x, y = rotate(x, y, line)
    else -- forward
      sx = sx + (x * line.num)
      sy = sy + (y * line.num)
    end
  end
  
  return math.abs(sx) + math.abs(sy)
end

function rotate(x, y, line)
    local m = line.dir == "L" and 1 or -1
    if line.num == "90" then
      return -y * m , x * m
    elseif line.num == "180" then
      return -x , -y 
    elseif line.num == "270" then
      return y * m, -x * m
    end
end

lines = lines("input")
print(part_one(lines))
print(part_two(lines))