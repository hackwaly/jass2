function sum takes integer from, integer to returns integer
    local integer result = 0
    local integer curr = from
    loop
        exitwhen curr > to
        set result = result + curr
        set curr = curr + 1
    endloop
    return result
endfunction

function main takes nothing returns integer
    return sum(0, 100)
endfunction
