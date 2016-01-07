// Let's create some evidence
let evidence = [1;0;0]

// Let's weight the evidence
let weight = [50; 25; 25]

// Create a threshold level above which a postive answer is given
let threshold = 40

// Model a perceptron
let perceptron evidence weight threshold =
    let weightedEvidence = 
        evidence
        |> List.zip weight
        |> List.fold (fun acc (e,w) -> acc + e * w) 0
    
    if weightedEvidence > threshold then 1 else 0

// Call the perceptron
perceptron evidence weight threshold 


