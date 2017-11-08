doubleMe x = x+x
--doubleUs x y = x*2 + y*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

doubleSmallNumber' x = (if x>100 then x else x*2) +1

conanO'Brian = "It's a-me, Conan O'Brian"

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
