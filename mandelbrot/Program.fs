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

type PixelLocation (x:int, y:int) =
    member this.X = x
    member this.Y = y

    member this.ToComplex (numericRange:ComplexRange) (pixelSize:float) =
        new ComplexPoint(
            numericRange.MinValue.Real + (pixelSize * (float x)), 
            numericRange.MinValue.Imaginary + (pixelSize * (float y))
        )

type MandelbrotRenderer (windowWidth:int, windowHeight:int, numericRange:ComplexRange, maxIterations:int) = 
    let pixelSize = 
        Operators.max
          (numericRange.RealRange / (float windowWidth)) 
          (numericRange.ImaginaryRange / (float windowHeight))

    let pixelArray = Array2D.init windowWidth windowHeight (fun x y -> new PixelLocation(x, y))
    let image = new Bitmap(windowWidth, windowHeight)

    member this.WindowHeight = windowHeight
    member this.WindowWidth = windowWidth
    member this.NumericRange = numericRange
    member this.PixelSize = pixelSize

    member this.DoesPixelEscape (pixelLocation:PixelLocation) (currentIteration:int) =
        let rec CalculateIteration (currentPointZ:ComplexPoint) (originalPointC:ComplexPoint) (currentIteration:int) =
            if currentIteration = maxIterations then 
                maxIterations
            else if currentPointZ.Magnitude > 2.0 then 
                currentIteration
            else
                let sqrZ = Complex.Multiply(currentPointZ, currentPointZ)
                let newPointZ = Complex.Add(sqrZ, originalPointC)
                CalculateIteration newPointZ originalPointC (currentIteration + 1)


        let ComplexPixel = pixelLocation.ToComplex this.NumericRange this.PixelSize
        let Iteration = CalculateIteration ComplexPoint.Zero ComplexPixel 0

        let InSet = Iteration = maxIterations
        not InSet

    member this.Render = 
        for x:int in 0..(this.WindowWidth - 1) do
            for y:int in 0..(this.WindowHeight - 1) do
                if (this.DoesPixelEscape pixelArray.[x, y] 0) then
                    image.SetPixel(x, y, Color.White)
                else
                    image.SetPixel(x, y, Color.Black)
        image

    member this.ToString = sprintf "{WindowHeight: %i, WindowWidth: %i, NumericRange: %s}" windowHeight windowWidth numericRange.ToString
    

[<EntryPoint>]
let main argv =
    let FullRange = ComplexRange(new ComplexPoint(-2.5, -1.0), new ComplexPoint(1.0, 1.0))
    let MandelbrotRenderer = new MandelbrotRenderer(3500, 2000, FullRange, 500)

    printfn "{FullRange: %s}" FullRange.ToString
    printfn "{MandelbrotRenderer: %s, PixelSize: %f}" MandelbrotRenderer.ToString MandelbrotRenderer.PixelSize

    MandelbrotRenderer.Render.Save(Path.Combine(__SOURCE_DIRECTORY__, "rendered.png"))

    0 // return an integer exit code



