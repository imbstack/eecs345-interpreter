description: Multiple function calls in an expression
value: 20

fact(n) {
  var f = 1;
  while (n > 1) {
    f = f * n;
    n = n - 1;
  }
  return f;
}

binom(a, b) {
  var val = fact(a) / (fact(b) * fact(a-b));
  return val;
}

main() {
  return binom(6,3);
}
