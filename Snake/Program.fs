// Learn more about F# at http://fsharp.org

open System

//type Snake(x,y,len) =
//    member this.x = x
//    member this.y = y
//    member this.length = len

type Snake = {x:int; y:int; length:int}

type Game(snake:Snake) = 
    member this.snake = snake
    member this.score = snake.length


type Frame(w,h,changes) = 
    member this.changes = changes
    member this.w = w
    member this.h = h
    member this.Write pos ch = Frame(w,h,changes)
    member this.Read pos = 'a'
    member this.PrintFrame (snake:Snake) =
        let toOneD x y w = y*w+x
        let snakeIdx = toOneD snake.x snake.y w
        let res = List.mapi (fun i -> if snakeIdx = i then (fun x -> 'x') else (fun x -> 'o') ) [0..w*h]
        for y in [0..h-1] do
            let sz  = y  * w
            res |> List.skip sz |> List.take w |> printfn "%A"
        ()


let moveSnake (snake:Snake) x y =
    {x = snake.x + x; y=snake.y + y; length=0}

let snakeEat food (snake:Snake) = 
    let hasFoodAtLocation = List.contains (snake.x,snake.y) food
    if(hasFoodAtLocation) then
        {x=snake.x;y=snake.y;length =snake.length + 1}
    else
        {x=snake.x;y=snake.y;length=snake.length}

let eatFood (snake:Snake) food = 
    List.except [(snake.x,snake.y)] food

let updateTail (snake:Snake) tail =
    let tailLength = List.length tail
    //List.init snake.length (fun x -> if x < tailLength then List.item x tail else (snake.x,snake.y))
    if snake.length > List.length tail then (snake.x,snake.y) :: tail else (snake.x,snake.y) :: List.tail tail
   
let createFrame w h ch =
    List.init (w*h) (fun x -> ch)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
