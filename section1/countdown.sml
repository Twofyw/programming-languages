fun countdown(x : int)=
  if x > 1
  then x :: countdown(x-1)
  else [1]
