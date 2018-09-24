// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Numerics
open System.DrawingCore

type ComplexPoint = Complex

type ComplexRange (minValue:ComplexPoint, maxValue:ComplexPoint) =
    member this.MinValue = minValue
    member this.MaxValue = maxValue
    member this.RealRange = maxValue.Real - minValue.Real
    member this.ImaginaryRange = maxValue.Imaginary - minValue.Imaginary
    member this.CenterValue = (maxValue + minValue) / new ComplexPoint(2.0, 0.0)
    member this.ToString = sprintf "{MinValue: %A, MaxValue: %A}" minValue maxValue

let getComplexPoint x y (numericRange:ComplexRange) pixelSize =
    new ComplexPoint(
        numericRange.MinValue.Real + (pixelSize * (float x)), 
        numericRange.MinValue.Imaginary + (pixelSize * (float y))
    )

type MandelbrotConfig (windowWidth:int, windowHeight:int, numericRange:ComplexRange, maxIterations:int) = 
    let pixelSize = 
        Operators.max
          (numericRange.RealRange / (float windowWidth)) 
          (numericRange.ImaginaryRange / (float windowHeight))
          
    member this.WindowHeight = windowHeight
    member this.WindowWidth = windowWidth
    member this.NumericRange = numericRange
    member this.PixelSize = pixelSize
    member this.MaxIterations = maxIterations
    member this.ToString = sprintf "{WindowHeight: %i, WindowWidth: %i, NumericRange: %s}" windowHeight windowWidth numericRange.ToString

type PixelInfo (escapeIteration:int, inSet:Boolean, color:Color)=
    member this.EscapeIteration = escapeIteration
    member this.InSet = inSet
    member this.Color=color

let GetPixelInfo (mandelbrotConfig:MandelbrotConfig) (complexPoint:ComplexPoint) (currentIteration:int) =
    let previousHashes = new System.Collections.Generic.HashSet<int>(100)

    let rec CalculateIteration (currentPointZ:ComplexPoint) (originalPointC:ComplexPoint) (currentIteration:int) =
        if currentIteration = mandelbrotConfig.MaxIterations then 
            currentIteration
        else if currentPointZ.Magnitude > 2.0 then 
            currentIteration
        else 
            let sqrZ = Complex.Multiply(currentPointZ, currentPointZ)
            let newPointZ = Complex.Add(sqrZ, originalPointC)
            // Perform Periodicity Check
            if previousHashes.Add(newPointZ.GetHashCode()) then
                CalculateIteration newPointZ originalPointC (currentIteration + 1)
            else
                mandelbrotConfig.MaxIterations

    let Iteration = CalculateIteration ComplexPoint.Zero complexPoint 0
    let InSet = Iteration = mandelbrotConfig.MaxIterations
    let color = 
        if(InSet) then Color.Black
        else
            let maxCValue=200.0
            let IterationPercent = (float Iteration) / (float mandelbrotConfig.MaxIterations) 
            let cvalue = int (IterationPercent * maxCValue)
            Color.FromArgb(cvalue, cvalue, cvalue)
    new PixelInfo(Iteration, InSet, color)

let RenderMandelbrot (mandelbrotConfig:MandelbrotConfig) =
    let image = new Bitmap(mandelbrotConfig.WindowWidth, mandelbrotConfig.WindowHeight)

    let setPixel x y =
        let complexPoint = getComplexPoint x y mandelbrotConfig.NumericRange mandelbrotConfig.PixelSize
        let pixelInfo = GetPixelInfo mandelbrotConfig complexPoint 0
        image.SetPixel(x, y, pixelInfo.Color)

    let setPixelRow y = async { 
        for x:int in 0..(mandelbrotConfig.WindowWidth - 1) do
            setPixel x y
    }

    [0..(mandelbrotConfig.WindowHeight - 1)] 
        |> List.map setPixelRow
        |> Async.Parallel
        |> Async.RunSynchronously 
        |> ignore

    image

[<EntryPoint>]
let main argv =
    let range = ComplexRange(new ComplexPoint(-2.5, -1.0), new ComplexPoint(1.0, 1.0))
    let imageWidth = 7000
    let imageHeight = 4000
    let maxIteration = 1000
    let imageFileName = sprintf "rendered_%ix%i_%i.png" imageWidth imageHeight maxIteration
    let mandelbrotConfig = new MandelbrotConfig(imageWidth, imageHeight, range, maxIteration)

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    let image = RenderMandelbrot mandelbrotConfig

    timer.Stop()
    let seconds = ((float timer.ElapsedMilliseconds)/1000.0)

    let output = (sprintf "MandelbrotRender finished:
        RenderTime: %fs
        ImageSize: %i x %i
        Range: %s
        MaxIterations: %i
        ImageName: %s" 
        seconds imageWidth imageHeight range.ToString maxIteration imageFileName)

    Console.WriteLine(output)

    let g = Graphics.FromImage(image)
    g.DrawString(output, 
        SystemFonts.DefaultFont,
        Brushes.White,
        0.0f,
        0.0f,
        StringFormat.GenericDefault)
    
    image.Save(Path.Combine(__SOURCE_DIRECTORY__, imageFileName))

    0 // return an integer exit code



