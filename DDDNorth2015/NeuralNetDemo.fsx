// This has to be evaluated every time
#load "..\packages\FsLab.0.3.8\FsLab.fsx"

// Evaluate this one time only to install the missing R packages
open RProvider.utils
R.install_packages("MASS")
R.install_packages("pbkrtest")
R.install_packages("lattice")
R.install_packages("Matrix")
R.install_packages("mgcv")
R.install_packages("grid")
R.install_packages("neuralnet")
R.install_packages("caret")
R.install_packages("zoo")

// Evaluate this every time to open the libraries we need
open Deedle
open RDotNet
open RProvider
open RProvider.``base``
open RProvider.datasets
open RProvider.neuralnet
open RProvider.caret

// Load the iris data 
let iris : Frame<int, string> = R.iris.GetValue()

// Let's see what the classifier thinks before training
let features =
    iris
    |> Frame.filterCols (fun c _ -> c <> "Species")
    |> Frame.mapColValues (fun c -> c.As<double>())
let targets =
    R.as_factor(iris.Columns.["Species"])
 
R.featurePlot(x = features, y = targets, plot = "pairs")

// Split 70/30 training/testing
iris.ReplaceColumn("Species", targets.AsNumeric())
let range = [1..iris.RowCount]
let trainingIdxs : int[] = R.sample(range, iris.RowCount*7/10).GetValue()
let testingIdxs : int[] = R.setdiff(range, trainingIdxs).GetValue()
let trainingSet = iris.Rows.[trainingIdxs]
let testingSet = iris.Rows.[testingIdxs]

// Let's train our neuralnet
let nn = 
    R.neuralnet(
        "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width", 
        data = trainingSet, hidden = R.c(3,2), 
        err_fct = "ce", linear_output = true)
 
// Plot the resulting neural network with coefficients
R.eval(R.parse(text="library(grid)"))
R.plot_nn nn

// Let's see how well we did...

// Split training into features and targets
let testingFeatures = 
    testingSet
    |> Frame.filterCols (fun c _ -> c <> "Species") 
    |> Frame.mapColValues (fun c -> c.As<double>())
let testingTargets = 
    testingSet.Columns.["Species"].As<int>().Values

// Let's get our training neural net to classify our new data
let prediction = 
    R.compute(nn, testingFeatures)
     .AsList().["net.result"].AsVector() 
    |> Seq.cast<double>
    |> Seq.map (round >> int)


// How many did we get wrong?
let misclassified = 
    Seq.zip prediction testingTargets
    |> Seq.filter (fun (a,b) -> a<>b)
    |> Seq.length
 
printfn "Misclassified irises '%d' of '%d'" misclassified (testingSet.RowCount)





