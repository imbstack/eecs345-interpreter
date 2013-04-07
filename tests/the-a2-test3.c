description: Blocks of code should have different scope
value: 2

var i = 2;
if (i == 2) {
  var i = 3;
  i = 4;
}
return i;
