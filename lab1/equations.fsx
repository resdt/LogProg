open System

// Dichotomy method
let dichotomy f a b tolerance =
    let rec bisect a b =
        let midpoint = (a + b) / 2.0
        let f_mid = f midpoint
        if abs(f_mid) < tolerance then midpoint
        else if f_mid * f a < 0.0 then bisect a midpoint
        else bisect midpoint b
    bisect a b

// Iteration method
let iterations phi x0 tolerance maxIter =
    let rec iterate x prevX count =
        let nextX = phi x
        if abs(nextX - prevX) < tolerance || count >= maxIter then nextX
        else iterate nextX x (count + 1)
    iterate x0 x0 0

// Newton's method
let newton f f' x0 tolerance maxIter =
    let rec newton_iter x count =
        let nextX = x - (f x) / (f' x)
        if abs(nextX - x) < tolerance || count >= maxIter then nextX
        else newton_iter nextX (count + 1)
    newton_iter x0 0

// 1
let f1 x = Math.Sqrt(1.0 - 0.4 * x**2.0) - Math.Asin(x)
let phi1 x = 0.5 * (Math.Sqrt(1.0 - 0.4 * x**2.0) + x)

// 2
let f2 x = Math.Exp(x) - Math.Exp(-x) - 2.0
let f2' x = Math.Exp(x) + Math.Exp(-x)
let phi2 x = Math.Log(2.0 + Math.Exp(-x))

// 3
let f3 x = Math.Sin(Math.Log(x)) - Math.Cos(Math.Log(x)) + 2.0 * Math.Log(x)
let f3' x = (Math.Cos(Math.Log(x)) / x) + (Math.Sin(Math.Log(x)) / x) + 2.0 / x
let phi3 x = Math.Exp((Math.Cos(Math.Log(x)) - Math.Sin(Math.Log(x))) / 2.0)

let main =
    let tol = 1e-6
    let maxIter = 1000

    let sol1_dichotomy = dichotomy f1 0.0 1.0 tol
    let sol1_iteration = iterations phi1 0.7 tol maxIter
    let sol1_newton = newton f1 (fun x -> -(0.4 * x) / Math.Sqrt(1.0 - 0.4 * x**2.0) - 1.0 / Math.Sqrt(1.0 - x**2.0)) 0.7 tol maxIter

    let sol2_dichotomy = dichotomy f2 0.0 1.0 tol
    let sol2_iteration = iterations phi2 0.7 tol maxIter
    let sol2_newton = newton f2 f2' 0.9 tol maxIter

    let sol3_dichotomy = dichotomy f3 1.0 3.0 tol
    let sol3_iteration = iterations phi3 1.5 tol maxIter
    let sol3_newton = newton f3 f3' 1.5 tol maxIter

    printfn "%10.5f  %10.5f  %10.5f" sol1_dichotomy sol1_iteration sol1_newton
    printfn "%10.5f  %10.5f  %10.5f" sol2_dichotomy sol2_iteration sol2_newton
    printfn "%10.5f  %10.5f  %10.5f" sol3_dichotomy sol3_iteration sol3_newton

main
