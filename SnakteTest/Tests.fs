module Tests

open System
open Xunit
open Program

[<Fact>]
let ``Snake can move`` () =
    let mySnake = {x=0;y=0;tail=[]}
    let moved = moveSnake mySnake 1 0
    Assert.True(moved.x = 1)
    Assert.True(moved.y = 0)

[<Fact>]
let ``Snake can eat and grow ``() =
    let food = [(1,0)]
    let snake = {x=0;y=0;tail=[]}
    let moved = moveSnake snake 1 0 
    let fed = snakeEat food moved
    
    Assert.Equal(1,fed.length) 

[<Fact>]
let ``Snake can eat only if position contains food ``() =
    let snake = {x=0;y=0;tail=[]}
    let moved = moveSnake snake 1 0 

    let food = [] 
    let fed = snakeEat food moved
    Assert.Equal(0,fed.length) 

[<Fact>]
let ``Eating food decreeses food amount ``() =
    let snake = {x=1;y=1;tail=[]}
    let newFood = eatFood snake [(1,1)]
    Assert.Equal(0,newFood.Length)

[<Fact>]
let `` Eating food increases player score ``() = 
    let snake = {x=1;y=1;tail=[]}
    let newSnake = snakeEat [(1,1)] snake
    let game = {snake=newSnake;food=(0,0)}

    Assert.Equal(1,game.score)

[<Fact>]
let `` moving the tails follows ``() =
    let snake = {x=1;y=1;tail=[(0,0);(0,0)]}
    let newTail = updateTail snake snake.tail
    Assert.True(List.length newTail = 2,"Verify the tail grows when snake length is larger than tail")

    let again = updateTail snake newTail
    Assert.True(List.length again = 2,"Verify that it only grows when snake is longer than tail")

    let movedTail = updateTail snake [(0,0);(1,0)]
    Assert.StrictEqual([(1,0); (1,1)] , movedTail)
    ()

[<Fact>]
let ``Game: The player scores by eating food ``() =
    let game = {snake={x=1;y=1;tail=[]};food=(2,1)}
    Assert.True(game.score = 0,"Start with 0 score")

    let state = updateGame game Move.Right
    Assert.True(state.score = 1,"i moved so i should have points")

    Assert.True((gameEat game (0,0)).score = 0,"no points where no food") 
    Assert.True((gameEat game (2,1)).score = 1,"Points where food") 

    Assert.True((gameEat game (0,0)).food = (2,1),"Food stays in position when not eaten")
    Assert.False((gameEat game (2,1)).food = (2,1),"Food moves when eaten")

    ()
    

