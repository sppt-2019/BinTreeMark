open System 
    
type tree =
    | Node of left : tree * right : tree
    | Leaf of int

let print str = printf "%A\n" str   

type Clock() =
    let mutable start = DateTime.Now
    member this.Start () = start = DateTime.Now
    member this.Check () = DateTime.Now.Subtract(start).Ticks

type SestoftRunner(func, args, message, duration) =
    let func = func
    let args = args
    let msg = message
    let dur = int64(duration * 10000)
    let clock = new Clock()
    
    let mutable aMean : double = 0.0
    let mutable sDeviation : double = 0.0
    
    let titleFormat = Printf.TextWriterFormat<string->string->string->unit>("%-20s \t %20s \t %20s")
    let outputFormat = Printf.TextWriterFormat<string->float->float->unit>("%-20s \t %20.2f \t %20.2f")
    let mean sum iterations =
        double(sum) / double(iterations)
    
    let standardDeviation (sumSqr:double) (mean:double) (itr:double) =
        if itr > 1.0 then
            sqrt ((sumSqr - mean * mean * itr) / (itr - 1.0))
        else 0.0
    
    member val ArithmeticMean = aMean with get, set                    
    member val Deviation = sDeviation with get, set
    
    member this.Run () =
        let rec RunWhile (start:DateTime) duration sum sumSqr iterations =
            if DateTime.Now.Subtract(start).Ticks < duration then
                clock.Start()
                let res = func(args)
                let time = clock.Check() + sum
                RunWhile start duration time (time * time) (iterations + 1L)
            else
                let m = mean sum iterations
                let sd = standardDeviation (double(sumSqr)) m (double(iterations))
                (m, sd)
        let resTuple = RunWhile DateTime.Now dur 0L 0L 0L
        this.ArithmeticMean <- fst resTuple
        this.Deviation <- snd resTuple
        printfn titleFormat "Name" "Arithmetic Mean" "Standard Deviation"
        printfn outputFormat msg this.ArithmeticMean this.Deviation

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
    let eagerRunner = new SestoftRunner(leaves, t, "Eager function", 250)
    (*let lazyRunner = new SestoftRunner(lazyLeaves, t, "Lazy function", 10000)*)
    
    eagerRunner.Run()
    (*print ("Lazy function:\t\t" + laazy.ToString())
    print ("Difference:\t\t" + (laazy - eager).ToString())*)
    0 // return an integer exit code