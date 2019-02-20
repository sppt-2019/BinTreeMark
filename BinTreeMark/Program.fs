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

let lazyLeaves a = lazy(leaves a)

let findDepth tree =
    let rec depthRunner tree depth =
        match tree with
        | Node(left, right) -> max (depthRunner left (depth + 1)) (depthRunner right (depth + 1)) 
        | Leaf _ -> (depth + 1)
    depthRunner tree 0
    
let rec createTree depth valueGenerator =
    match depth with
    | 1 -> Leaf (valueGenerator())
    | n -> Node(left = (createTree (depth - 1) valueGenerator), right = (createTree (depth - 1) valueGenerator))

[<EntryPoint>]
let main argv =
    print "Building Tree"
    let t = createTree 5 getRandomNumber
    print "Commencing benchmark"
    let eagerRunner = new SestoftRunner<tree, int list>(leaves, t, "Eager function", 250)
    let lazyRunner = new SestoftRunner<tree, Lazy<int list>>(lazyLeaves, t, "Lazy function", 250)
    
    eagerRunner.Run()
    lazyRunner.Run()
    print ("Difference:\t\t\t" + (lazyRunner.ArithmeticMean - eagerRunner.ArithmeticMean).ToString())
    let lst = [727.7;1086.5;1091.0;1361.3;1490.5;1956.1]
    0 // return an integer exit code