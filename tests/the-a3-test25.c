description: Assignment side effects with functions
value: 20332

var x;

f(a,b) {
  return a * 100 + b;
}

fib(f) {
  var last = 0;
  var last1 = 1;

  while (f > 0) {
    f = f - 1;
    var temp = last1 + last;
    last = last1;
    last1 = temp;
  }
  return last;
}

main() {
  var y;
  var z = f(x = fib(3), y = fib(4));
  return z * 100 + y * 10 + x;
}
