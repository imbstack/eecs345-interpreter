description: While loops' nested scope works
value: 4

var i = 0;
while (i < 4) {
  var j = 0;
  j = j + 1;
  i = i + j;
}
return i;
