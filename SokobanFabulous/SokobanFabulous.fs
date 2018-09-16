namespace SokobanFabulous

open System
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Essentials
open Xamarin.Forms
open Pathfinding

module Game = 
    type Msg = 
        | TryFindPath of Levels.Point
        | TryPush of Levels.Point
        | FollowPath
        | Reset
        | LoadLevel of int
        | LevelComplete of Direction * int

    let initModel levelNumber =
        let level = Levels.levels.[levelNumber]
        let lines = level.Split '\n' |> Array.filter(not << String.IsNullOrWhiteSpace) |> Array.map (fun line -> line.TrimStart()) |> List.ofArray
        let map = loadMapFromString levelNumber lines

        { level=levelNumber; width=lines.[0].Length; height=lines.Length; 
          map=map.map; playerPos=map.playerPos; path=[]; pushableTreasure=None; 
          goalSquares = map.goalSquares;
          maximumLevelReached=levelNumber; }

    let init level = initModel level, Cmd.none

    let followPathCmd = 
        async { do! Async.Sleep 100
                return FollowPath }
        |> Cmd.ofAsyncMsg

    let levelCompleteCmd direction rotations=
        async { do! Async.Sleep 200
                return LevelComplete (direction, rotations) }
        |> Cmd.ofAsyncMsg

    let getTileSize model =
        let width = DeviceDisplay.ScreenMetrics.Width / DeviceDisplay.ScreenMetrics.Density
        let height = DeviceDisplay.ScreenMetrics.Height/ DeviceDisplay.ScreenMetrics.Density
        let buttonWidth = 4 // Allocate 4 x width of grid cell for buttons
        let h = height / (double model.height)
        let w = width / (double (model.width + buttonWidth))
        min h w

    let pushableTreasure model =
        let adjacentTreasureSquares =
            getNeighboursOf model model.playerPos
            |> List.choose(fun (p, xdelta, ydelta, direction) ->
                   match model.map.[p] with
                   | Treasure _ -> Some (p, xdelta, ydelta)
                   | _ -> None)

        adjacentTreasureSquares
        |> List.choose(fun (p, xdelta, ydelta) ->
            let oppositePoint = { x=model.playerPos.x + xdelta + xdelta; y=model.playerPos.y + ydelta + ydelta }
            match model.map.[oppositePoint] with
            | Empty | Goal -> Some p
            | _ -> None)

    let playSound (soundName:string) =
        let player = Plugin.SimpleAudioPlayer.CrossSimpleAudioPlayer.Current
        let assembly = typeof<Msg>.Assembly
        let stream = assembly.GetManifestResourceStream("SokobanFabulous.Sounds." + soundName);
        player.Load stream |> ignore
        player.Play()

    let update msg model =
        let newModel, cmd =
            match msg with
            | TryFindPath point ->
                // Try and calculate a path to move the player to the requested point
                let path = pathTo model model.playerPos point
                match path with
                | Some path' -> { model with path=path' }, followPathCmd
                | None -> model, Cmd.none
            | TryPush treasurePoint ->
                // Try and push treasure into an empty spot or the goal
                let emptySpot =
                    getNeighboursOf model treasurePoint
                    |> List.tryPick(fun (p, xdelta, ydelta, direction) ->
                        let oppositePoint = { x=treasurePoint.x + xdelta * -1; y=treasurePoint.y + ydelta * -1 }

                        match model.map.[p], model.map.[oppositePoint] with
                        | Empty, Player _ -> Some (p, Treasure NotOnTarget, direction)
                        | Goal, Player _ -> Some (p, Treasure OnTarget, direction)
                        | _ -> None)

                match emptySpot with
                | Some (emptySpot', treasure, direction) ->
                    playSound "push_box.mp3"
                    let vacantSquare = if model.goalSquares.Contains model.playerPos then Goal else Empty
                    let newMap = model.map.Add(emptySpot', treasure).Add(treasurePoint, Player direction).Add(model.playerPos, vacantSquare)
                    let levelCompleted = model.goalSquares |> Set.forall(fun s -> match newMap.[s] with Treasure _ -> true | _ -> false)

                    let cmd =
                        if levelCompleted then
                            // Start the winning animation
                            levelCompleteCmd direction 1
                        else
                            Cmd.none
                    { model with playerPos=treasurePoint; map=newMap}, cmd
                | None -> model, Cmd.none
            | FollowPath ->
                // Move player to the next point on the calculated path
                match model.path with
                | h::t -> 
                    let point, direction = h
                    let previousSquareContent = if model.goalSquares.Contains model.playerPos then Goal else Empty
                    let newMap = model.map.Add(point, Player direction).Add(model.playerPos, previousSquareContent)
                    playSound "walk.mp3"
                    { model with playerPos=point; map = newMap; path=t; }, followPathCmd
                | [] -> model, Cmd.none
            | Reset -> model, Cmd.ofMsg (LoadLevel model.level)
            | LoadLevel level ->
                let newModel, cmd = init level
                // preserve the maximum level reached
                { newModel with maximumLevelReached=model.maximumLevelReached }, cmd
            | LevelComplete (direction, rotations) ->
                let rotatedDirection =
                    match direction with
                    | Up -> Right
                    | Right -> Down
                    | Down -> Left
                    | Left -> Up
                let newMap = model.map.Add(model.playerPos, Player rotatedDirection)
                let cmd, level =
                    if rotations = 8 then
                        // Finish winning spin and load next level
                        Cmd.ofMsg (LoadLevel (model.level+1)), model.level + 1
                    else
                        // Spin some more
                        levelCompleteCmd rotatedDirection (rotations+1), model.level
                { model with map = newMap; maximumLevelReached = max level model.maximumLevelReached }, cmd

        match pushableTreasure newModel with
        | [ onlyOne ] -> { newModel with pushableTreasure = Some onlyOne }, cmd
        | _ -> { newModel with pushableTreasure = None }, cmd

    let squareImage = function
        | Player Left -> "player_12"
        | Player Right -> "player_09"
        | Player Up -> "player_06"
        | Player Down -> "player_03"
        | Empty -> "environment_15"
        | Wall -> "block_06" 
        | Dead -> "outside_96"
        | Goal -> "environment_09"
        | Treasure _ -> "crate_45"

    let view (model: Map) dispatch =
        // Fit to screen
        let tileSize = getTileSize model

        let getImageForSquare sq row col =
            let command =
                match sq with
                | Treasure _ -> TryPush { y=row; x = col } 
                | _ -> TryFindPath { y=row; x = col }

            View.Image(gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch (command))) ])
                .ImageSource(squareImage sq)
                .GridRow(row)
                .GridColumn(col)
                .BackgroundColor(Color.FromHex "#3C3C3C")

        let button text command =
            // button with padded text
            View.StackLayout(horizontalOptions=LayoutOptions.FillAndExpand,
                children = [
                    View.Button(text=text, command=(fun() -> dispatch command), textColor=Color.White, backgroundColor=Color.Gray)
                ])

        View.ContentPage(
            View.StackLayout(orientation=StackOrientation.Horizontal,
                children = [
                    View.Grid(rowSpacing=0.0, columnSpacing=0.0, rowdefs=[ for i in 1 .. model.height do yield (box tileSize) ], coldefs=[ for i in 1 .. model.width do yield (box tileSize)],
                        children = [ 
                            for row in 0 .. model.height-1 do
                                for col in 0 .. model.width-1 do
                                    let sq = model.map.[{y=row; x = col}]
                                    yield getImageForSquare sq row col
                        ])

                    View.StackLayout(orientation=StackOrientation.Vertical, horizontalOptions=LayoutOptions.FillAndExpand, spacing=20.0,
                        children = [
                            yield View.Label(text=sprintf "Level %d" (model.level + 1), textColor=Color.White)
                            yield button "Restart" Reset

                            if model.level < 48 && model.level < model.maximumLevelReached then
                                yield button "Next Level" (LoadLevel (model.level + 1))

                            if model.level > 0 then
                                yield button "Prev Level" (LoadLevel (model.level - 1))

                            if model.pushableTreasure.IsSome then
                                yield button "Push" (TryPush model.pushableTreasure.Value)
                        ])
                ])
        ).BackgroundColor(Color.Black)
    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram (fun() -> init 0) update view
