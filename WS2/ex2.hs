classifyBMI :: Float -> Float -> String
classifyBMI weight height =
    let bmi = (weight / (height ^ 2))
    in
    if bmi < 18.5 then "underweight"
        else if (bmi >= 18.5 && bmi<25) then "normal weight"
        else if (bmi >= 25 && bmi<30) then "overweight"
        else "obese"