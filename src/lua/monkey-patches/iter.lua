local dummy = vim.iter(ipairs {})

---@class Iter
---@field takewhile fun(self: Iter, p: fun(...: any): boolean): Iter
---@field dropwhile fun(self: Iter, p: fun(...: any): boolean): Iter
local mt = getmetatable(dummy)

mt.__index.takewhile = function(self, p)
  local finished = false
  return self:filter(function(...)
    if finished then
      return false
    elseif p(...) then
      return true
    else
      finished = true
      return false
    end
  end)
end

mt.__index.dropwhile = function(self, p)
  local dropping = true
  return self:filter(function(...)
    if dropping and p(...) then
      return false
    else
      dropping = false
      return true
    end
  end)
end
