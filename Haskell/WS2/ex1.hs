classify :: Int -> String
classify x  = if x <= 9 then "failed"
    else if x <= 12 then "passed"
    else if x <= 15 then "good"
    else if x <= 18 then "very good"
    else "excellent"


