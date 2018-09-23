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

let DoesPixelEscape (mandelbrotConfig:MandelbrotConfig) (complexPoint:ComplexPoint) (currentIteration:int) =
    let rec CalculateIteration (currentPointZ:ComplexPoint) (originalPointC:ComplexPoint) (currentIteration:int) =
        if currentIteration = mandelbrotConfig.MaxIterations then 
            currentIteration
        else if currentPointZ.Magnitude > 2.0 then 
            currentIteration
        else
            let sqrZ = Complex.Multiply(currentPointZ, currentPointZ)
            let newPointZ = Complex.Add(sqrZ, originalPointC)
            CalculateIteration newPointZ originalPointC (currentIteration + 1)


    let Iteration = CalculateIteration ComplexPoint.Zero complexPoint 0

    let InSet = Iteration = mandelbrotConfig.MaxIterations
    not InSet

let RenderMandelbrot (mandebrotConfig:MandelbrotConfig)= 
    let image = new Bitmap(mandebrotConfig.WindowWidth, mandebrotConfig.WindowHeight)
    for x:int in 0..(mandebrotConfig.WindowWidth - 1) do
        for y:int in 0..(mandebrotConfig.WindowHeight - 1) do
            let complexPoint = getComplexPoint x y mandebrotConfig.NumericRange mandebrotConfig.PixelSize
            if (DoesPixelEscape mandebrotConfig complexPoint 0) then
                image.SetPixel(x, y, Color.White)
            else
                image.SetPixel(x, y, Color.Black)
    image


[<EntryPoint>]
let main argv =
    let fullRange = ComplexRange(new ComplexPoint(-2.5, -1.0), new ComplexPoint(1.0, 1.0))
    let mandelbrotConfig = new MandelbrotConfig(3500, 2000, fullRange, 500)

    printfn "{FullRange: %s}" fullRange.ToString
    printfn "{MandelbrotRenderer: %s, PixelSize: %f}" mandelbrotConfig.ToString mandelbrotConfig.PixelSize

    (RenderMandelbrot mandelbrotConfig).Save(Path.Combine(__SOURCE_DIRECTORY__, "rendered.png"))

    0 // return an integer exit code



