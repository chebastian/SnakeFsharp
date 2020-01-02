// Learn more about F# at http://fsharp.org

open System

let rand = System.Random()
let rand2 min max = (rand.Next(min,max),rand.Next(min,max))

type Snake = {x:int; y:int; tail:list<int*int>} with
    member this.length = List.length this.tail

type Game =  {snake:Snake; food:int*int} with
    member this.score = this.snake.length


type Frame(w,h,changes) = 
    member this.changes = changes
    member this.w = w
    member this.h = h
    member this.Write pos ch = Frame(w,h,changes)
    member this.Read pos = 'a'
    member this.PrintFrame (snake:Snake) tail =
        let toOneD x y w = y*w+x
        let snakeIdx = toOneD snake.x snake.y w
        let foodIdx = (toOneD (fst tail) (snd tail) w)
        let snakeIndexes = List.map (fun s -> toOneD (fst s) (snd s) w) snake.tail
        let isSnake i =  if i = snakeIdx then true else false || List.contains i snakeIndexes
        let res = List.mapi (fun i -> if (isSnake i) || i = foodIdx then (fun x -> 'x') else (fun x -> '.') ) [0..w*h]
        let action state x = 
            let d = state |> List.skip (x*w) |> List.take w |> List.map (fun d -> printf "%c" d)
            printfn "%s" ""
            ()

        [0..h-1] |> List.map (fun x -> action res x) |> ignore



let moveSnake (snake:Snake) x y =
    {x = snake.x + x; y=snake.y + y; tail=snake.tail}

let snakeEatAlt (snake:Snake) = 
    {x=snake.x;y=snake.y;tail=snake.tail @ [(snake.x,snake.y)]}
    
let snakeEat food (snake:Snake) = 
    let hasFoodAtLocation = List.contains (snake.x,snake.y) food
    if(hasFoodAtLocation) then
        {x=snake.x;y=snake.y;tail=(snake.x,snake.y)::snake.tail}
    else
        {x=snake.x;y=snake.y;tail=snake.tail}

let eatFood (snake:Snake) food = 
    List.except [(snake.x,snake.y)] food

let updateTail (snake:Snake) tail =
    if List.length tail > 0 then
        if snake.length > List.length tail then (snake.x,snake.y) :: tail else (List.tail tail) @ [(snake.x,snake.y)]
    else
        tail
   
let createFrame w h ch =
    List.init (w*h) (fun x -> ch)

let rec readLines () = seq{
    let line = Console.ReadKey().KeyChar
    if line = 'q'then
        yield string line
    else
        if line <> ' ' then
            yield string line
            yield! readLines()
}

type Move = 
|Left|Right|Up|Down|None

let stringToTypeMove str = 
    match str with
    | "w" -> Move.Up
    | "a" -> Move.Left
    | "s" -> Move.Down
    | "d" -> Move.Right
    | _ -> Move.None


let stringToMove move snake = 
    match move with
    | Move.Up -> moveSnake snake 0 -1
    | Move.Left -> moveSnake snake -1 0
    | Move.Down -> moveSnake snake 0 1
    | Move.Right -> moveSnake snake 1 0
    | Move.None -> moveSnake snake 0 0


let gameEat (state:Game) (pos:(int*int)) =
    if state.food = pos then
        {snake=snakeEatAlt state.snake;food=rand2 0 8}
    else
        {snake=state.snake;food=state.food} 

let updateGame (state:Game) (move:Move) = 
    let moved = stringToMove move state.snake
    let gg = gameEat state (moved.x, moved.y)
    let newSnake = {x=moved.x;y=moved.y;tail=updateTail gg.snake gg.snake.tail}
    {snake=newSnake;food=gg.food}
    

[<EntryPoint>]
let main argv =
    let mutable snake = {x=0;y=0;tail=[(0,0); (0,0)]}
    let frame = new Frame(10,10,[])
    let mutable originalTail = updateTail snake snake.tail
    frame.PrintFrame snake (3,3)
    let mutable game = {snake=snake;food=(3,3)}

    for i in readLines() do
        
        game <- updateGame game (stringToTypeMove i)
        frame.PrintFrame game.snake game.food
    0 // return an integer exit code
