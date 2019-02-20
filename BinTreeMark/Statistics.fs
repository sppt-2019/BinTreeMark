module BinTreeMark.Statistics

let realStandardDeviation (lst:double list) (mean:double) =
    let rec numarator (lst:double list) (mean:double) (sum:double) =
        if List.length(lst) > 0 then
            let xi = List.head lst
            let newsum = sum + pown (xi - mean) 2
            numarator (List.tail lst) mean newsum
         else sum
    let num = numarator lst mean 0.0
    sqrt (num / double(List.length(lst) - 1))
    
let standardDeviation (lst:double list) (mean:double) (iterations:int64) =
    let sqdList = (List.map (fun x -> x * x) lst)
    let deltaTimeSquared = (List.sum sqdList)
    sqrt ((deltaTimeSquared - mean * mean * double(iterations)) / double(iterations - 1L));
    
let mean (sum:double) (iterations:double) =
        sum / iterations