-- Find all occurrences of a pattern in the string,
-- return a list of starting and ending indices.
---@param pattern string pattern to be searched
---@param overlap boolean? allow overlapping patterns? default false
---@return [integer,integer][]
function string:find_all(pattern, overlap)
  local matches = {}
  local start_pos = 1
  overlap = overlap or false

  while true do
    ---@diagnostic disable-next-line: param-type-mismatch
    local start_idx, end_idx = self:find(pattern, start_pos)
    if not start_idx then break end
    table.insert(matches, { start_idx, end_idx })
    if overlap then
      start_pos = start_idx + 1
    else
      start_pos = end_idx + 1
    end
  end

  return matches
end
