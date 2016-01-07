// Model a perceptron
let perceptron weight threshold evidence =
    let weightedEvidence = 
        evidence
        |> List.zip weight
        |> List.fold (fun acc (e,w) -> acc + e * w) 0
    
    if weightedEvidence > threshold then [1] else [0]

// Model a layer
[1]
|> perceptron [40] 45

// Model a network

// Layer 2
|> perceptron [30] 40

// Layer 3
|> perceptron [25] 20

// Layer 4
|> perceptron [35] 25
 


