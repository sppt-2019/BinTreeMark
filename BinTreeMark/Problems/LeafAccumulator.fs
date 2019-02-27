module BinTreeMark.Problems.LeafAccumulator

open System.Threading.Tasks

type tree =
    | Node of left : tree * right : tree
    | Leaf of int

let getRandomNumber () =
    let rnd = System.Random()
    rnd.Next(100)

let leaves a =
    let rec leavesAccum n leaves =
        match n with
        | (Leaf i) -> i :: leaves
        | Node (left, right) -> leavesAccum left (leavesAccum right leaves)
    leavesAccum a []

let lazyLeaves a =
    let lst = lazy(leaves a)
    lst.Value

let rec createTree depth valueGenerator =
    match depth with
    | 1 -> Leaf (valueGenerator())
    | n -> Node(left = (createTree (depth - 1) valueGenerator), right = (createTree (depth - 1) valueGenerator))

let division leaves =
    let x = System.Random().Next(1, leaves)
    let y = leaves - x
    (x, y)
    
let rec createRandomTree leaves valueGenerator =
    let div = division leaves
    match leaves with
    | 1 -> Leaf (valueGenerator())
    | n -> Node(left = (createRandomTree (fst div) valueGenerator), right = (createRandomTree (snd div) valueGenerator))

let asyncLeaves t =
    let rec asyncLeavesAccum (t:tree) (leaves:int list) = async {
        match t with
        | (Leaf i) -> return i::leaves
        | Node (left, right) -> return Async.RunSynchronously (asyncLeavesAccum left (Async.RunSynchronously(asyncLeavesAccum right leaves)))
    }
    Async.RunSynchronously (asyncLeavesAccum t [])

let parallelLeaves t =
    let rec asyncAccum (t:tree) = async {        
        match t with
        | (Leaf i) -> return [i]
        | Node (left, right) -> return [(asyncAccum left); (asyncAccum right)]
                                       |> Async.Parallel
                                       |> Async.RunSynchronously
                                       |> List.concat
    }
    Async.RunSynchronously (asyncAccum t)

let tplParallelLeaves t = 
    let rec leavesAccum n =
        match n with
        | (Leaf i) -> [i] 
        | Node (left, right) -> List.append
                                    (Task.Factory.StartNew<int list> (fun () ->  leavesAccum left)).Result
                                    (Task.Factory.StartNew<int list> (fun () ->  leavesAccum right)).Result
                                
    leavesAccum t

let lazyAsyncLeaves t =
    let lst = lazy(asyncLeaves t)
    lst.Value

let lazyParallel t =
    let lst = lazy(parallelLeaves t)
    lst.Value
    
let lazyTPLLeaves t =
    let lst = lazy(tplParallelLeaves t)
    lst.Value