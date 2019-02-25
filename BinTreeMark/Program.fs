open System
open BinTreeMark.Mark8
open BinTreeMark.Statistics
type tree =
    | Node of left : tree * right : tree
    | Leaf of int

let print str = printf "%A\n" str

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

let lazyAsyncLeaves a =
    let lst = lazy(asyncLeaves a)
    lst.Value

let lazyParallel a =
    let lst = lazy(parallelLeaves a)
    lst.Value

[<EntryPoint>]
let main argv =
    printfn "Building Tree"
    let t = createTree 5 getRandomNumber
    printfn "%A" t
    printfn "Setting up benchmarks"
    let eagerRunner = new SestoftRunner<tree, int list>(leaves, t, "Eager Sequential", 250)
    let lazyRunner = new SestoftRunner<tree, int list>(lazyLeaves, t, "Lazy Sequential", 250)
    let eagerAsyncRunner = new SestoftRunner<tree, int list>(asyncLeaves, t, "Eager Async", 250)
    let lazyAsyncRunner = new SestoftRunner<tree, int list>(lazyAsyncLeaves, t, "Lazy Async", 250)
    let eagerParaRunner = new SestoftRunner<tree, int list>(parallelLeaves, t, "Eager Parallel", 250)
    let lazyParaRunner = new SestoftRunner<tree, int list>(lazyParallel, t, "Lazy Parallel", 250)
    
    printfn "Running benchmarks"
    eagerRunner.Run()
    lazyRunner.Run()
    eagerAsyncRunner.Run()
    lazyAsyncRunner.Run()
    eagerParaRunner.Run()
    lazyParaRunner.Run()
    
    printfn "Results"
    printfn "%s" eagerRunner.TitleFormat
    printfn "%s" (eagerRunner.Result())
    printfn "%s" (lazyRunner.Result())
    printfn "%s" (eagerAsyncRunner.Result())
    printfn "%s" (lazyAsyncRunner.Result())
    printfn "%s" (eagerParaRunner.Result())
    printfn "%s" (lazyParaRunner.Result())
    
    
    let lst = [727.7;1086.5;1091.0;1361.3;1490.5;1956.1]
    0 // return an integer exit code