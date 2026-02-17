-- Definizione del produttore come coroutine
function produttore()
    for n = 1, 5 do
        print("Produttore: genera numero " .. n)
        coroutine.yield(n)  -- Sospende e passa il valore
    end
    print("Produttore: finito")
end

-- Definizione del consumatore come coroutine
function consumatore(prod_co)
    while true do
        local status, valore = coroutine.resume(prod_co)
        if not status or coroutine.status(prod_co) == "dead" then
            break
        end
        print("  Consumatore: riceve valore " .. valore .. " e lo processa")
    end
    print("  Consumatore: finito")
end

-- Esempio 1: Produttore-Consumatore
print("=== Esempio Produttore-Consumatore ===")
local co_prod = coroutine.create(produttore)
consumatore(co_prod)

print("\n=== Esempio 2: Generatore di Fibonacci ===")
-- Esempio più avanzato: generatore di numeri Fibonacci
function fibonacci()
    local a, b = 0, 1
    while true do
        coroutine.yield(a)
        a, b = b, a + b
    end
end

local fib = coroutine.create(fibonacci)

-- Genera i primi 10 numeri di Fibonacci
for i = 1, 10 do
    local status, num = coroutine.resume(fib)
    print("Fibonacci[" .. i .. "] = " .. num)
end

print("\n=== Esempio 3: Pipeline di Coroutine ===")
-- Pipeline: genera -> filtra -> consuma
function generatore()
    for i = 1, 20 do
        coroutine.yield(i)
    end
end

function filtro(gen_co)
    return coroutine.wrap(function()
        while true do
            local status, val = coroutine.resume(gen_co)
            if not status or coroutine.status(gen_co) == "dead" then
                break
            end
            -- Filtra solo i numeri pari
            if val % 2 == 0 then
                coroutine.yield(val)
            end
        end
    end)
end

local gen = coroutine.create(generatore)
local filt = filtro(gen)

print("Numeri pari da 1 a 20:")
for num in filt do
    print("  " .. num)
end