module BinTreeMark.TestRunners

open System
open BinTreeMark.Statistics

type Clock() =
    let mutable start = System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Start () = start <- System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Check () = (System.Diagnostics.Stopwatch.GetTimestamp() - start)

let titleFormat = sprintf "%-30s \t %20s \t %20s" "Name" "Arithmetic Mean" "Standard Deviation"

type SestoftRunner<'T, 'U>(func:'T -> 'U, args:'T, message, duration:TimeSpan) =
    let func = func
    let args = args
    let msg = message
    let clock = new Clock()
    
    let mutable aMean : double = Double.NaN
    let mutable sDeviation : double = Double.NaN
    
    member val ArithmeticMean = aMean with get, set                   
    member val Deviation = sDeviation with get, set
   
    member this.Result () = sprintf "%-30s \t %20.2f \t %20.2f" msg this.ArithmeticMean this.Deviation
    
    member this.Run () =
        let rec RunWhile (start:DateTime) (dur:TimeSpan) (sum:double) (times:double list) (iterations:int64) =
            if DateTime.UtcNow.Subtract(start) < dur then
                clock.Start()
                let res = func args
                let time = double(clock.Check())
                let newTimes = List.append times [time]
                RunWhile start dur (time + sum) newTimes (iterations * 2L)
            else
                let m = mean sum (double(iterations))
                let sd = standardDeviation times m iterations
                (m, sd)
        let resTuple = RunWhile DateTime.Now duration 0.0 [] 0L
        this.ArithmeticMean <- fst resTuple
        this.Deviation <- snd resTuple

type MorellRunner<'T, 'P, 'U>(func:'T -> 'U, problems:'P list, problemGenerator:'P -> 'T, message:string, repitions:int64) =
    let func = func
    let msg = message
    let probs = problems
    let reps = repitions
    let clock = new Clock()
    
    let mutable aMean : double = Double.NaN
    let mutable results : string list = []
    
    member val ArithmeticMean = aMean with get, set                   
    member val Results = results with get, set
    
    member this.Result () = sprintf "%-30s \t %20.2f" msg this.ArithmeticMean
    
    member this.Run () =
        let mutable i = 0
        for prob in probs do
            let rec RunWhile (reps:int64) (sum:double) (times:double list) (iterations:int64) =
                if iterations < reps then
                    let problem = problemGenerator prob 
                    clock.Start()
                    let res = func problem
                    let time = double(clock.Check())
                    let newTimes = List.append times [time]
                    RunWhile reps (time + sum) newTimes (iterations + 1L)
                else
                    let m = mean sum (double(iterations))
                    m
            let m = RunWhile reps 0.0 [] 0L
            this.ArithmeticMean <- m
            this.Results <- List.append this.Results [this.Result()]
            i <- i + 1
            printf "%s: %d/%d" msg i probs.Length
            System.Console.SetCursorPosition(0, System.Console.CursorTop)
        printfn ""

type McCollinRunner<'T, 'U>(func:'T -> 'U, problems:'T list, message:string, repitions:int64) =
    let func = func
    let msg = message
    let probs = problems
    let reps = repitions
    let clock = new Clock()
    
    let mutable aMean : double = Double.NaN
    let mutable sDeviation : double = Double.NaN
    let mutable results : string list = []
    
    member val ArithmeticMean = aMean with get, set                   
    member val Deviation = sDeviation with get, set
    member val Results = results with get, set
    
    member this.Result () = sprintf "%-30s \t %20.2f \t %20.2f" msg this.ArithmeticMean this.Deviation
    
    member this.Run () =
        let mutable i = 0
        for problem in probs do
            let rec RunWhile (reps:int64) (sum:double) (times:double list) (iterations:int64) =
                if iterations < reps then
                    clock.Start()
                    let res = func problem
                    let time = double(clock.Check())
                    let newTimes = List.append times [time]
                    RunWhile reps (time + sum) newTimes (iterations + 1L)
                else
                    let m = mean sum (double(iterations))
                    let sd = standardDeviation times m iterations
                    (m, sd)
            let resTuple = RunWhile reps 0.0 [] 0L
            this.ArithmeticMean <- fst resTuple
            this.Deviation <- snd resTuple
            this.Results <- List.append this.Results [this.Result()]
            i <- i + 1
            printf "%s: %d/%d" msg i probs.Length
            System.Console.SetCursorPosition(0, System.Console.CursorTop)
        printfn ""