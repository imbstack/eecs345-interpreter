description: While loops with continue should work
value: 10

var i = 0;
var j = 10;
while (j > 0) {
  i = i + 1;
  j = j - 1;
  continue;
  j = j + 1;
}
return i;
