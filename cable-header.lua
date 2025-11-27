-- cable-header.lua

function Pandoc(doc)
  local meta = doc.meta

  -- 1. Helper function to safely get metadata as a string
  local function get_meta(key)
    if meta[key] then
      return pandoc.utils.stringify(meta[key])
    else
      return nil
    end
  end

  -- 2. Helper function to build the table row
  local function get_field_row(key, label)
    local value = get_meta(key)
    if value and value ~= "" then
      return [[
        <tr class="cable-row">
          <td class="cable-label-cell"><strong>]] .. label .. [[:</strong></td>
          <td class="cable-value-cell">]] .. value .. [[</td>
        </tr>
      ]]
    end
    return ""
  end

  -- 3. Generate the HTML Table Content
  local html_content = [[
    <div class="cable-metadata-container">
      <table class="cable-table">
      ]] .. get_field_row('cable-date', 'Date') .. [[
      ]] .. get_field_row('cable-from', 'From') .. [[
      ]] .. get_field_row('cable-to', 'To') .. [[
      ]] .. get_field_row('cable-info', 'Info') .. [[
      ]] .. get_field_row('cable-subject', 'Subject') .. [[
      ]] .. get_field_row('cable-tags', 'TAGS') .. [[
      ]] .. get_field_row('cable-ref', 'Ref') .. [[
      </table>
    </div>
    <hr>
  ]]

  -- 4. TRAVERSAL: Look for the Div with class "cable-meta" and replace it
  for i, block in ipairs(doc.blocks) do
    if block.t == "Div" and block.classes:includes("cable-meta") then
       -- Replace the placeholder Div with our HTML Block
       doc.blocks[i] = pandoc.RawBlock('html', html_content)
       -- Stop after finding the first one
       break
    end
  end

  return doc
end