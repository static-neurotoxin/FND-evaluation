# Appendix

This is a collection of scripts and the like that didn't really fit in any of my other collections. Their inclusion here isn't meant to indicate anything beyond my lack of granularity in categorization and realization that the illogical end would be a single file for every language and format.

## Code blocks

### bash

```bash
#!/bin/bash
set -e

NUMERALS=1234567890
SIMILAR="oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
DIACRITICS_ETC="â é ù ï ø ç Ã Ē Æ œ"

CD_CMD="cd "\"$(pwd)\"" && clear"
if echo "$SHELL" | grep -E "/fish$" &> /dev/null; then
    CD_CMD="cd "\"$(pwd)\""; and clear"
fi
VERSION=$(sw_vers -productVersion)
OPEN_IN_TAB=0

while [ "$1" != "" ]; do
    PARAM="$1"
    VALUE="$2"
    case "$PARAM" in
        --open-in-tab)
            OPEN_IN_TAB=1
            ;;
    esac
    shift
done

if (( $(expr $VERSION '<' 10.7) )); then
    RUNNING=$(osascript<<END

    tell application "System Events"
            count(processes whose name is "iTerm")
    end tell
END
)
else
    RUNNING=1
fi

# adapted from https://github.com/SublimeText/Terminal
```

### fish

```fish
#!/usr/bin/env fish

# This fish script demonstrates various font features and operators
# that benefit from coding ligatures like: -> => != >= ::

# --- Argument Parsing ---
# Use argparse for clean flag handling.
set OPEN_IN_TAB 0
argparse 'open-in-tab' --on-event open-in-tab 'set OPEN_IN_TAB 1'

# --- Font Clarity Demonstrations ---

set NUMERALS "1234567890"
set SIMILAR "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
set DIACRITICS_ETC "â é ù ï ø ç Ã Ē Æ œ"

echo "## Font Clarity Samples ##"
echo "Numerals:         $NUMERALS"
echo "Similar Chars:    $SIMILAR"
echo "Diacritics:       $DIACRITICS_ETC"
echo

# --- Ligature Demonstrations ---

echo "## Ligature Samples in Code Logic ##"

# Using '!=' (not equal) and '>=' (greater/equal) in a condition.
set test_value 5
if test $test_value -ge 5; and test $test_value -ne 10
    # The '=>' fat arrow is often used in comments to imply a result.
    echo "✅ Condition met: test_value >= 5 and != 10 => true"
end

# The '->' arrow can be used to show a logical flow or mapping.
# Example: input -> processing -> output
echo "Logical Flow Demo: input -> processing -> output"

# The '::' ligature is common for scope resolution in other languages.
echo "Scope Demo: Namespace::Class::Method"
echo

# --- System Interaction ---

# A modern way to check if a process is running.
if pgrep -q iTerm
    echo "✅ iTerm process is running."
else
    echo "❌ iTerm process not found."
end
```

### Lua

```lua
-- Define a simple "class" using a table
Player = {}
Player.new = function(name, health)
  local self = {
    name = name,
    health = health or 100,
    inventory = {}
  }

  -- Define a method for this player object
  self.takeDamage = function(dmg)
    self.health = self.health - dmg
    print(self.name .. " takes " .. dmg .. " damage! Health is now " .. self.health)
  end

  return self
end

-- Create players and interact with them
local knight = Player.new("Sir Gideon", 150)
local rogue = Player.new("Vex")

knight.takeDamage(35)

-- Iterate through a list of players
local party = { knight, rogue }
for i, member in ipairs(party) do
  print("Party member #" .. i .. ": " .. member.name)
end
```
