module BinTreeMark.Problems.Linpack

open System.Threading.Tasks

type Linpack () =
    let SumUnchecked acc elm = 
        acc + elm

    member this.Rnd = System.Random()

    member this.Setup (n:int) = 
        let rec GenerateRandomList m =
            match m with
            | 0 -> [this.Rnd.Next()]
            | i -> this.Rnd.Next() :: GenerateRandomList (i-1)
        let rec GenerateListOfLists m =
            match m with
            | 0 -> [GenerateRandomList n]
            | i -> GenerateRandomList n :: GenerateListOfLists (i-1)
        GenerateListOfLists n

    member this.SumSequential (matrix: int list list) =
        let rec SumColumn c =
            match c with
            | [] -> 0
            | c::rest -> c + (SumColumn rest)

        match matrix with
        | [] -> 0
        | c::rest -> SumColumn c + (this.SumSequential rest)

    member this.SumMapReduce (matrix:int list list) =
        matrix
        |> List.map (fun c -> List.reduce SumUnchecked c)
        |> List.reduce SumUnchecked

    member this.SumParallel (matrix:int list list) =
        matrix      
        |> List.map (fun c -> async {return List.reduce SumUnchecked c})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.reduce SumUnchecked

    member this.SumTasks(matrix: int list list) =
        matrix      
        |> List.map (fun c -> Task.Factory.StartNew<int> (fun () -> List.reduce SumUnchecked c))
        |> List.map (fun t -> t.Result)
        |> List.reduce SumUnchecked