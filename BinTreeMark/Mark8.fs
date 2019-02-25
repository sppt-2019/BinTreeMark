module BinTreeMark.Mark8

open System
open BinTreeMark.Statistics

type Clock() =
    let mutable start = System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Start () = start <- System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Check () = (System.Diagnostics.Stopwatch.GetTimestamp() - start)

type SestoftRunner<'T, 'U>(func:'T -> 'U, args:'T, message, duration) =
    let func = func
    let args = args
    let msg = message
    let dur = int64(duration * 10000)
    let clock = new Clock()
    
    let mutable aMean : double = 0.0
    let mutable sDeviation : double = 0.0
    
    member val ArithmeticMean = aMean with get, set                   
    member val Deviation = sDeviation with get, set
    
    member val TitleFormat = sprintf "%-30s \t %20s \t %20s" "Name" "Arithmetic Mean" "Standard Deviation"
    member this.Result () = sprintf "%-30s \t %20.2f \t %20.2f" msg this.ArithmeticMean this.Deviation
    
    member this.Run () =
        let rec RunWhile (start:DateTime) (duration:int64) (sum:double) (times:double list) (iterations:int64) =
            if DateTime.Now.Subtract(start).Ticks < duration then
                clock.Start()
                let res = func args
                let time = double(clock.Check())
                let newTimes = List.append times [time]
                RunWhile start duration (time + sum) newTimes (iterations + 1L)
            else
                let m = mean sum (double(iterations))
                let sd = standardDeviation times m iterations
                (m, sd)
        let resTuple = RunWhile DateTime.Now dur 0.0 [] 0L
        this.ArithmeticMean <- fst resTuple
        this.Deviation <- snd resTuple
