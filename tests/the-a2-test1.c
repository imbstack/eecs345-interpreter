description: GCD should work
value: 7

var a = 14;
var b = 3 * a - 7;
if (a < b) {
  var temp = a;
  a = b;
  b = temp;
}
var r = a % b;
while (r != 0) {
  a = b;
  b = r;
  r = a % b;
}
return b;
