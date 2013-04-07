description: While loops with break should work
value: 5

var i = 3;
while (i < 10) {
  if (i == 5)
    break;
  else
    i = i + 1;
}
return i;
