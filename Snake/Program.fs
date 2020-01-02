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
    member this.PrintFrame (snake:Snake) tail =
        let toOneD x y w = y*w+x
        let snakeIdx = toOneD snake.x snake.y w
        let snakeIndexes = List.map (fun s -> toOneD (fst s) (snd s) w) tail
        let isSnake i =  if i = snakeIdx then true else false || List.contains i snakeIndexes
        let res = List.mapi (fun i -> if isSnake i then (fun x -> 'x') else (fun x -> '.') ) [0..w*h]
        let action state x = 
            let d = state |> List.skip (x*w) |> List.take w |> List.map (fun d -> printf "%c" d)
            printfn "%s" ""
            ()

        [0..h-1] |> List.map (fun x -> action res x) |> ignore



let moveSnake (snake:Snake) x y =
    {x = snake.x + x; y=snake.y + y; length=snake.length}

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
    //if snake.length > List.length tail then (snake.x,snake.y) :: tail else (snake.x,snake.y) :: List.tail tail
    if snake.length > List.length tail then (snake.x,snake.y) :: tail else (List.tail tail) @ [(snake.x,snake.y)]
   
let createFrame w h ch =
    List.init (w*h) (fun x -> ch)

let rec readLines () = seq{
    let line = Console.ReadLine()
    if line = "q" then
        yield line
    else
        if line <> null then
            yield line
            yield! readLines()
}

let stringToMove str snake = 
    match str with
    | "w" -> moveSnake snake 0 -1
    | "a" -> moveSnake snake -1 0
    | "s" -> moveSnake snake 0 1
    | "d" -> moveSnake snake 1 0
    | _ ->      moveSnake snake 0 0
    

[<EntryPoint>]
let main argv =
    let mutable snake = {x=0;y=0;length=4}
    let frame = new Frame(10,10,[])
    let mutable originalTail = updateTail snake []
    frame.PrintFrame snake 

    for i in readLines() do
        let move = stringToMove i snake
        let newTail = updateTail snake originalTail
        frame.PrintFrame move newTail
        snake <- move
        originalTail <- newTail
    0 // return an integer exit code
