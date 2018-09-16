module Pathfinding

let sqr x = float(x * x) //Square two ints into a float
let distance a b = sqrt (sqr(a.x+b.x) + sqr(a.y+b.y))

//The A* Algorithm
let rec aStar value g h neighbours goal start (openNodes: 'a list) (closedNodes: 'a list) =
    let f x:float = (g x)+(h x) //f will be the value we sort open nodes buy.
    let isShorter nodeA nodeB = nodeA = nodeB && f nodeA < f nodeB 
            
    let rec checkNeighbours neighbours openNodeAcc = 
        match neighbours with
        | [] -> openNodeAcc
        | currentNode::rest ->
            let likeCurrent = fun n -> (value n) = (value currentNode) //value of n == value of current
            let containsCurrent = List.exists likeCurrent              //list contains likeCurrent
            let checkNeighbours = checkNeighbours rest 

            if openNodeAcc |> List.exists (isShorter currentNode) then //The current node is a shorter path than than one we already have.
                let shorterPath = openNodeAcc |> List.filter (not << likeCurrent) //So remove the old one...
                checkNeighbours  (currentNode::shorterPath)   //...and arry on with the new one.
            elif not(containsCurrent closedNodes) && not(containsCurrent openNodeAcc) then //The current node has not been queried
                checkNeighbours (currentNode::openNodeAcc) //So add it to the open set
            else checkNeighbours openNodeAcc // else carry on

    let nodes = neighbours openNodes.Head //The next set of nodes to work on
    
    let pathToGoal = nodes |> List.tryFind (fun x -> (value x) = goal) 
    if pathToGoal.IsSome then pathToGoal //Found the goal!
    else
        let nextSet = 
            checkNeighbours nodes openNodes.Tail
            |> List.sortBy f //sort open set by node.f
        if nextSet.Length > 0 then //Carry on pathfinding
            aStar value g h neighbours goal start nextSet (nextSet.Head::closedNodes)
        else None //if there are no op

//a wrapper for mapPoint that can contain pathing data as per typical A* pathfinding
type PathingNode =  
    { point:Point; direction:Direction; h:float; g:float; parent:PathingNode option;} //g = cost of path so far, h = estimated cost to goal, parent = tile we came here from                


let getNeighboursOf (map:Map) p =  //return list of map points that surround current map point
    [ for xdelta, ydelta, direction in [(-1, 0, Left); (1, 0, Right); (0, -1, Up); (0, 1, Down)] do
          let x, y = p.x+xdelta, p.y+ydelta
          if (y >= 0 && x >= 0 && x < map.width && y < map.height) //bounds checking
              then yield { x=x; y=y }, xdelta, ydelta, direction ]

//returns a pathnode based on a given map point
let pointToPathNode parent goal node direction = { point=node; direction=direction; h=distance node goal; g=(parent.g+1.0); parent=Some(parent) } 

//A 2D tile specific version of the A* algorithm
let pathFind (map:Map) (goal:Point) =
    aStar (fun n-> n.point) (fun n-> n.g) (fun n-> n.h) (fun n -> (getNeighboursOf map n.point)
                                                                   |> List.filter(fun (p, _, _, _) ->  //let s = map.map.[p.y, p.x]
                                                                                    match map.map.[{x=p.x; y=p.y}] with
                                                                                    | Empty -> true 
                                                                                    | Goal -> true 
                                                                                    | _ -> false)
                                                                   |> List.map (fun (p, _, _, direction) -> pointToPathNode n goal p direction)) goal

let rec readPath (path:PathingNode) (list:(Point * Direction) list) =
    match path.parent with
    | None -> list
    | Some parent ->
        match parent.parent with
        | Some p -> readPath parent ((parent.point, parent.direction)::list)
        | None -> list // stop before we get to the last item which equals the start point

let pathTo map a b =
    let path = pathFind map b a [{point=a; direction=Left; h=distance a b;g=0.0;parent=None}] []
    path |> Option.map(fun path' -> readPath path.Value [path.Value.point, path.Value.direction]) 
