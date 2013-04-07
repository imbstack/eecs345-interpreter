description: While loops with break and continue should work
value: 5

var i = 0;
var j = 10;
while (j > 0) {
  i = i + 1;
  j = j - 1;
  if (i == 5)
    break;
  continue;
  j = j + 1;
}
return i;
