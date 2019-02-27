open BinTreeMark.TestRunners
open BinTreeMark.Problems.LeafAccumulator
open BinTreeMark.Problems.Linpack
open System

type OS =
        | OSX            
        | Windows
        | Linux

let getOS = 
        match int Environment.OSVersion.Platform with
        | 4 | 128 -> Linux
        | 6       -> OSX
        | _       -> Windows


[<EntryPoint>]
let main argv =
    printfn "Setting up benchmarks"
    let l = new Linpack ()
    let sumProblems = List.map (fun s -> pown 2 s) [1..12]
    let probs = [createTree 1 getRandomNumber; createTree 2 getRandomNumber; createTree 3 getRandomNumber; createTree 4 getRandomNumber; createTree 5 getRandomNumber]                                                                                                                                    getRandomNumber]

    let sumSeqRunner = new MorellRunner<int list list, int, int>(l.SumSequential, sumProblems, l.Setup, "Matrix Sum Sequential", 100L)
    let sumMaReRunner = new MorellRunner<int list list, int, int>(l.SumMapReduce, sumProblems, l.Setup, "Matrix Sum Map Reduce", 100L)
    let sumPaRunner = new MorellRunner<int list list, int, int>(l.SumParallel, sumProblems, l.Setup, "Matrix Sum Parallel", 100L)    
    let eagerRunner = new McCollinRunner<tree, int list>(leaves, probs, "Eager Sequential", 100L)
    let lazyRunner = new McCollinRunner<tree, int list>(lazyLeaves, probs, "Lazy Sequential", 100L)
    let eagerAsyncRunner = new McCollinRunner<tree, int list>(asyncLeaves, probs, "Eager Async", 100L)
    let lazyAsyncRunner = new McCollinRunner<tree, int list>(lazyAsyncLeaves, probs, "Lazy Async", 100L)
    let eagerParaRunner = new McCollinRunner<tree, int list>(parallelLeaves, probs, "Eager Parallel", 100L)
    let lazyParaRunner = new McCollinRunner<tree, int list>(lazyParallel, probs, "Lazy Parallel", 100L)
    let tplParallelRunner = new McCollinRunner<tree, int list>(tplParallelLeaves, probs, "Eager TPL Parallel", 100L)
    let lazyTPLParallelRunner = new McCollinRunner<tree, int list>(lazyTPLLeaves, probs, "Lazy TPL Parallel", 100L)
    
    printfn "Running benchmarks"
    sumSeqRunner.Run()
    sumMaReRunner.Run()    
    sumPaRunner.Run()
    eagerRunner.Run()
    lazyRunner.Run()
    eagerAsyncRunner.Run()
    lazyAsyncRunner.Run()
    eagerParaRunner.Run()
    lazyParaRunner.Run()
    tplParallelRunner.Run()
    lazyTPLParallelRunner.Run()
    
    printfn ""
    printfn "Results"
    printfn "%s" titleFormat
    printfn "%s" (sumSeqRunner.Result())
    printfn "%s" (sumMaReRunner.Result())
    printfn "%s" (sumPaRunner.Result())
    printfn "%s" (eagerRunner.Result())
    printfn "%s" (lazyRunner.Result())
    printfn "%s" (eagerAsyncRunner.Result())
    printfn "%s" (lazyAsyncRunner.Result())
    printfn "%s" (eagerParaRunner.Result())
    printfn "%s" (lazyParaRunner.Result())
    printfn "%s" (tplParallelRunner.Result())
    printfn "%s" (lazyTPLParallelRunner.Result())
    
    if getOS = Windows then
        System.Console.ReadLine()
        0 // return an integer exit code
    else
        0 // return an integer exit code
