description: Function call in the parameter of a function
value: 24

fact(n) {
  var r = 1;
  while (n > 1) {
    r = r * n;
    n = n - 1;
  }
  return r;
}

main() {
  return fact(fact(3) - fact(2));
}
