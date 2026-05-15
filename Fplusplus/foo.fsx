open type System.Console

let add =
    WriteLine "init add"
    fun x y -> x + y

let closure =
    let n = 10
    fun x -> x + n
    
WriteLine(closure 32)
WriteLine (add 40 2)