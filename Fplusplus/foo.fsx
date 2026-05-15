open type System.Console

let add =
    WriteLine "init add"
    fun x y -> x + y

let closure =
    let n = 10
    fun x -> x + n
    
let doot () =
    let mutable n = 10
    n <- 20
    let y = n
    y
    
let counter () =
    let mutable count = 0
    fun () ->
        count <- count + 1
        count
    
WriteLine(closure 32)

WriteLine (add 40 2)

let c = counter ()

for i in 1..10 do
    WriteLine (c ())
    
WriteLine (c ())