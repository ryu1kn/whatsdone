
function hashCode(str) { // java String#hashCode
  var hash = 0;
  for (var i = 0; i < str.length; i++) {
    hash = str.charCodeAt(i) + ((hash << 5) - hash);  // eslint-disable-line no-bitwise
  }
  return hash;
}

function intToARGB(i) {
  /* eslint-disable no-bitwise */
  return ((i >> 24) & 0xFF).toString(16) +
         ((i >> 16) & 0xFF).toString(16) +
         ((i >> 8) & 0xFF).toString(16) +
          (i & 0xFF).toString(16);
  /* eslint-enable no-bitwise */
}

// Taken from http://stackoverflow.com/questions/3426404/create-a-hexadecimal-colour-based-on-a-string-with-javascript
export const getColorCode = str => {
  const code = intToARGB(hashCode(str));
  return '#' + code.substr(0, 6);
}
