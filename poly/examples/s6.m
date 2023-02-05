let val f = (fn x => x + 1) in
  let val g = (fn x => x + 2) in
    f = g
  end
end
