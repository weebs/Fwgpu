open System

// let print (s: string) = Console.WriteLine s
let print x = Console.WriteLine (string x)

let a = 40
let b = 2
let c = a + b
Console.WriteLine (string c)

let mutable n = 0
while n < 10 do
    n <- n + 1
    
for i in 1..10 do
    n <- n + i
// todo : printfn $"{n}"
print n