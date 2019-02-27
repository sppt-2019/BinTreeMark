open BinTreeMark.TestRunners
open BinTreeMark.Problems.LeafAccumulator
open BinTreeMark.Problems.Linpack

[<EntryPoint>]
let main argv =
    let l = new Linpack ()
    
    printfn "Building Linpack matrices"
    let sumProblems = List.map (fun s -> l.Setup(pown 2 s)) [1..12]
    let sumSeqRunner = new MorellRunner<int list list, int>(l.SumSequential, sumProblems, "Matrix Sum Sequential", 100L)
    let sumMaReRunner = new MorellRunner<int list list, int>(l.SumMapReduce, sumProblems, "Matrix Sum Map Reduce", 100L)
    let sumPaRunner = new MorellRunner<int list list, int>(l.SumParallel, sumProblems, "Matrix Sum Parallel", 100L)

    printfn "Running Linpack summation"
    sumSeqRunner.Run()
    sumMaReRunner.Run()
    sumPaRunner.Run()

    printfn ""
    printfn "Results"
    printfn "%s" sumSeqRunner.TitleFormat
    printfn "%s" (sumSeqRunner.Result())
    printfn "%s" (sumMaReRunner.Result())
    printfn "%s" (sumPaRunner.Result())
    

    printfn "Building Tree"
    let bt1 = createTree 1 getRandomNumber
    let bt2 = createTree 2 getRandomNumber
    let bt3 = createTree 3 getRandomNumber
    let bt4 = createTree 4 getRandomNumber
    let bt5 = createTree 5 getRandomNumber
    let probs = [bt1; bt2; bt3; bt4; bt5]
    
    printfn ""
    printfn "Setting up benchmarks"
    let eagerRunner = new MorellRunner<tree, int list>(leaves, probs, "Eager Sequential", 100L)
    let lazyRunner = new MorellRunner<tree, int list>(lazyLeaves, probs, "Lazy Sequential", 100L)
    let eagerAsyncRunner = new MorellRunner<tree, int list>(asyncLeaves, probs, "Eager Async", 100L)
    let lazyAsyncRunner = new MorellRunner<tree, int list>(lazyAsyncLeaves, probs, "Lazy Async", 100L)
    let eagerParaRunner = new MorellRunner<tree, int list>(parallelLeaves, probs, "Eager Parallel", 100L)
    let lazyParaRunner = new MorellRunner<tree, int list>(lazyParallel, probs, "Lazy Parallel", 100L)
    let tplParallelRunner = new MorellRunner<tree, int list>(tplParallelLeaves, probs, "Eager TPL Parallel", 100L)
    let lazyTPLParallelRunner = new MorellRunner<tree, int list>(lazyTPLLeaves, probs, "Lazy TPL Parallel", 100L)
    
    printfn ""
    printfn "Running benchmarks"
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
    printfn "%s" eagerRunner.TitleFormat
    printfn "%s" (eagerRunner.Result())
    printfn "%s" (lazyRunner.Result())
    printfn "%s" (eagerAsyncRunner.Result())
    printfn "%s" (lazyAsyncRunner.Result())
    printfn "%s" (eagerParaRunner.Result())
    printfn "%s" (lazyParaRunner.Result())
    printfn "%s" (tplParallelRunner.Result())
    printfn "%s" (lazyTPLParallelRunner.Result())
    
    System.Console.ReadLine()
    0 // return an integer exit code
