grade :: [a] -> String
grade ""  = "Enter a valid mark "
grade x = if x >= 70 then "1" else if x <70 then "2.1"
  else if x <60 then "2.2" else if x < 50 then "3"
    else "fail"
