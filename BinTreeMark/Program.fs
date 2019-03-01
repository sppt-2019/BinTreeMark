open BinTreeMark.TestRunners
open BinTreeMark.Problems.LeafAccumulator
open BinTreeMark.Problems.Linpack
open System
open System.IO

type OS =
        | OSX            
        | Windows
        | Linux

let getOS = 
        match int Environment.OSVersion.Platform with
        | 4 | 128 -> Linux
        | 6       -> OSX
        | _       -> Windows

let OpenFile fileName =
    if File.Exists(fileName) then
        File.Delete(fileName)
    let file = new StreamWriter(fileName)
    file

[<EntryPoint>]
let main argv =
    printfn "Setting up benchmarks"
    let l = new Linpack ()
    
    printfn "Generating matrices"
    let matrixSizes = List.map (fun b-> pown 2 b) [1..12]
    let sumProblems = List.map (fun s -> (s, l.Setup(s))) matrixSizes
    printfn "Generating binary trees"
    let probs = List.map (fun b -> (b, createTree b getRandomNumber)) [1..7]
    
    let sumSeqRunner = new McCollinRunner<int list list, int>(l.SumSequential, sumProblems, "Matrix Sum Sequential", 100L)
    let sumMaReRunner = new McCollinRunner<int list list, int>(l.SumMapReduce, sumProblems, "Matrix Sum Map Reduce", 100L)
    let sumPaRunner = new McCollinRunner<int list list, int>(l.SumParallel, sumProblems, "Matrix Sum Parallel", 100L)
    let sumTaRunner = new McCollinRunner<int list list, int>(l.SumTasks, sumProblems, "Matrix Sum Tasks", 100L)
    let eagerRunner = new McCollinRunner<tree, int list>(leaves, probs, "Eager Sequential", 100L)
    let lazyRunner = new McCollinRunner<tree, int list>(lazyLeaves, probs, "Lazy Sequential", 100L)
    let eagerParaRunner = new McCollinRunner<tree, int list>(parallelLeaves, probs, "Eager Parallel", 100L)
    let lazyParaRunner = new McCollinRunner<tree, int list>(lazyParallel, probs, "Lazy Parallel", 100L)
    let tplParallelRunner = new McCollinRunner<tree, int list>(tplParallelLeaves, probs, "Eager TPL Parallel", 100L)
    let lazyTPLParallelRunner = new McCollinRunner<tree, int list>(lazyTPLLeaves, probs, "Lazy TPL Parallel", 100L)
    
    printfn "Running benchmarks"
    sumSeqRunner.Run()
    sumMaReRunner.Run()    
    sumPaRunner.Run()
    sumTaRunner.Run()
    eagerRunner.Run()
    lazyRunner.Run()
    eagerParaRunner.Run()
    lazyParaRunner.Run()
    tplParallelRunner.Run()
    lazyTPLParallelRunner.Run()
    
    printfn ""
    printfn "Done, writing files"

    let file = OpenFile "fsharp-linpack.csv"
    file.WriteLine "Problem Size,Sequential,Sequential Error,Map Reduce,Map Reduce Error,Parallel,Parallel Error,Tasks,Tasks Error"

    for res in sumSeqRunner.Results do
        let ma = sumMaReRunner.Results.[res.Key]
        let pa = sumPaRunner.Results.[res.Key]
        let ta = sumTaRunner.Results.[res.Key]
        let line = (sprintf "%d,%f,%f,%f,%f,%f,%f,%f,%f" res.Key (fst res.Value) (snd res.Value) (fst ma) (snd ma) (fst pa) (snd pa) (fst ta) (snd ta))

        file.WriteLine line
    file.Flush ()
    file.Close ()

    let file = OpenFile "fsharp-accumulate.csv"
    file.WriteLine "Problem Size,Eager Sequential,Eager Sequential Error,Lazy Sequential,Lazy Sequential Error,Eager Async,Eager Async Error,Lazy Async,Lazy Async Error,Eager Parallel,Eager Parallel Error,Lazy Parallel,Lazy Parallel Error,Eager TPL,Eager TPL Error,Lazy TPL,Lazy TPL Error"

    for eagerSeq in eagerRunner.Results do
        let lazySeq = lazyRunner.Results.[eagerSeq.Key]
        let eagerPar = eagerParaRunner.Results.[eagerSeq.Key]
        let lazyPar = lazyParaRunner.Results.[eagerSeq.Key]
        let eagerTPL = tplParallelRunner.Results.[eagerSeq.Key]
        let lazyTPL = lazyTPLParallelRunner.Results.[eagerSeq.Key]
        let line = (sprintf "%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f" eagerSeq.Key (fst eagerSeq.Value) (snd eagerSeq.Value) 
            (fst lazySeq) (snd lazySeq)
            (fst eagerPar) (snd eagerPar)
            (fst lazyPar) (snd lazyPar)
            (fst eagerTPL) (snd eagerTPL)
            (fst lazyTPL) (snd lazyTPL))

        file.WriteLine line
    file.Flush ()
    file.Close ()

    printfn "Files be written"
    
    if getOS = Windows then
        ignore(System.Console.ReadLine())
        0 // return an integer exit code
    else
        0 // return an integer exit code
