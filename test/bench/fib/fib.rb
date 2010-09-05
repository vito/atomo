def fib(n)
  if n == 0
    1
  elsif n == 1
    1
  else
    fib(n - 2) + fib(n - 1)
  end
end

before = Time.now
puts fib(25)
after = Time.now
puts (after - before)
