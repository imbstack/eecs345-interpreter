description: Varaiables declared inside while loops aren't accessible outside
value: ERROR

var i = 0;
while (i < 4) {
  var j = 1;
  i = i + j;
}
return j;
