let val x = malloc "1" in
  let val y = malloc "2" in
    write (x = y)
  end
end
