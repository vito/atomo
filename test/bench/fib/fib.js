function fib(n) {
    if (n < 2) {
        return 1;
    } else {
        return fib(n - 2) + fib(n - 1);
    }
}

var before = (new Date()).getTime();
console.log(fib(20));
var after = (new Date()).getTime();
console.log(after - before);
