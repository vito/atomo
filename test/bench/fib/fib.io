Number fib := method(
    if(self == 0,
        1,
        if(self == 1,
            1,
            (self - 2) fib + (self - 1) fib))
)

t1 := Date clone now
20 fib println
Date clone now secondsSince(t1) println
