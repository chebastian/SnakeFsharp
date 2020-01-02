module Tests

open System
open Xunit
open Program

[<Fact>]
let ``Snake can move`` () =
    let mySnake = {x=0;y=0;length=0}
    let moved = moveSnake mySnake 1 0
    Assert.True(moved.x = 1)
    Assert.True(moved.y = 0)

[<Fact>]
let ``Snake can eat and grow ``() =
    let food = [(1,0)]
    let snake = {x=0;y=0;length=0}
    let moved = moveSnake snake 1 0 
    let fed = snakeEat food moved
    
    Assert.Equal(1,fed.length) 

[<Fact>]
let ``Snake can eat only if position contains food ``() =
    let snake = {x=0;y=0;length=0}
    let moved = moveSnake snake 1 0 

    let food = [] 
    let fed = snakeEat food moved
    Assert.Equal(0,fed.length) 

[<Fact>]
let ``Eating food decreeses food amount ``() =
    let snake = {x=1;y=1;length=0}
    let newFood = eatFood snake [(1,1)]
    Assert.Equal(0,newFood.Length)

[<Fact>]
let `` Eating food increases player score ``() = 
    let snake = {x=1;y=1;length=0}
    let newSnake = snakeEat [(1,1)] snake
    let game = new Game(newSnake)

    Assert.Equal(1,game.score)

[<Fact>]
let `` moving the tails follows ``() =
    let snake = {x=1;y=1;length=2}
    let newTail = updateTail snake [(0,1)]
    Assert.True(List.length newTail = 2,"Verify the tail grows when snake length is larger than tail")

    let again = updateTail snake newTail
    Assert.True(List.length again = 2,"Verify that it only grows when snake is longer than tail")

    let movedTail = updateTail snake [(0,0);(1,0)]
    Assert.StrictEqual([(1,1); (1,0)] , movedTail)
    ()
    

