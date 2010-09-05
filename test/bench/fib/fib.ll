define i32 @fib(i32 %a) {
    %stop = icmp ult i32 %a, 2
    br i1 %stop, label %bottom, label %continue

bottom:
    ret i32 1

continue:
    %atmp = sub i32 %a, 2
    %btmp = sub i32 %a, 1
    %n1 = call i32 @fib(i32 %atmp)
    %n2 = call i32 @fib(i32 %btmp)
    %addtmp = add i32 %n1, %n2
    ret i32 %addtmp
}

declare i32 @printf(i8*, ...)

@fmt = internal constant [4 x i8] c"%d\0A\00"

define i32 @main() {
    %res = call i32 @fib(i32 20)
    %str = getelementptr [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...)* @printf(i8* %str, i32 %res)
    ret i32 0
}
