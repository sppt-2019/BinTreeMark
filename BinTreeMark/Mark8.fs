module BinTreeMark.Mark8

open System
open BinTreeMark.Statistics

type Clock() =
    let mutable start = System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Start () = start <- System.Diagnostics.Stopwatch.GetTimestamp()
    member this.Check () = (System.Diagnostics.Stopwatch.GetTimestamp() - start)

type SestoftRunner<'T, 'U>(func:'T -> 'U, args:'T, message, duration:TimeSpan) =
    let func = func
    let args = args
    let msg = message
    let clock = new Clock()
    let durationNs:int64 = int64(duration.Milliseconds) * 1000L
    
    let mutable aMean : double = 0.0
    let mutable sDeviation : double = 0.0
    
    member val ArithmeticMean = aMean with get, set                   
    member val Deviation = sDeviation with get, set
    
    member val TitleFormat = sprintf "%-30s \t %20s \t %20s" "Name" "Arithmetic Mean" "Standard Deviation"
    member this.Result () = sprintf "%-30s \t %20.2f \t %20.2f" msg this.ArithmeticMean this.Deviation
    
    member this.Run () =
        let rec ExecuteNTimes (n:int64) =
            if n = 1L then
                func args
            else
                let res = ExecuteNTimes (n - 1L)
                func args
        
        let rec RunWhile (dur:int64) (iterations:int64) (deltaTime:double) (deltaTimeSquared:double) (totalCount:int64) =
            (* if DateTime.Now.Subtract(start) < dur then *)
            clock.Start()
            let res = ExecuteNTimes iterations
            let time = clock.Check()
            
            if time < dur && iterations < int64(Int32.MaxValue / 2) then
                RunWhile dur (iterations * 2L) (double(time) + deltaTime) (deltaTimeSquared + double(time * time)) (totalCount + iterations)
            else
                let m = double(time) / double(iterations)
                let sd = Math.Sqrt((deltaTimeSquared - m * m * double(iterations)) / double(iterations - 1L))
                (m, sd)
        let resTuple = RunWhile durationNs 5L 0.0 0.0 0L
        this.ArithmeticMean <- fst resTuple
        this.Deviation <- snd resTuple
