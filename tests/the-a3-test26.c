description: The straggler test
value: 115

var a = 10;
var b = 20;

bmethod() {
  var b = 30;
  return a + b;
}

cmethod() {
  var a = 40;
  return bmethod() + a + b;
}

main () {
  var b = 5;
  return cmethod() + a + b;
}

