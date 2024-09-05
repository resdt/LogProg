open System


let calcFunc x = 1.0 / (4.0 - x**4.0)

// Naive
let taylr_naive x maxSteps =
    let rec sum_terms step totalSum =
        if step > maxSteps then totalSum
        else
            let nxtTerm = (x**(4.0 * float step)) / (4.0 ** float (step + 1))
            sum_terms (step + 1) (totalSum + nxtTerm)
    sum_terms 1 (1.0 / 4.0)

// Smart
let taylr_smart x toleranse =
    let rec sum_terms_smart step totalSum prevTerm =
        if abs prevTerm < toleranse then (totalSum, step)
        else
            let nextTerm = prevTerm * (x**4.0) / 4.0
            sum_terms_smart (step + 1) (totalSum + nextTerm) nextTerm
    sum_terms_smart 1 (1.0 / 4.0) ((x**4.0) / 4.0)

let main =
    let start = 0.0
    let endPt = 1.0
    let numPoints = 10
    let tol = 1e-19
    let maxSteps = 1000

    printfn "%5s  %20s  %10s  %20s  %10s  %20s" "x" "Naive Taylr" "# Iter" "Smart Taylr" "# Iter" "Func Value"

    for indx = 0 to numPoints do
        let x_val = start + (float indx) / (float numPoints) * (endPt - start)

        let naiveVal = taylr_naive x_val maxSteps
        let smartVal, smartSteps = taylr_smart x_val tol
        let realFuncVal = calcFunc x_val

        printfn "%5.2f  %20.16f  %10d  %20.16f  %10d  %20.16f"
            x_val naiveVal maxSteps smartVal smartSteps realFuncVal

main
