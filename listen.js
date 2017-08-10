// this file is used to keep a image alive so that it can be pre-warmed and communicated with.
const net = require('net'),
      execSync = require('child_process').execSync,
      fs = require('fs');

// if the script is available, it will call it
if (fs.existsSync('/runner/prewarm.sh')) {
  console.log(execSync('sh /runner/prewarm.sh').toString());
}

// Creates a new TCP server. The handler argument is automatically set as a listener for the 'connection' event
var server = net.createServer(function(socket) {
  // Every time someone connects, tell them hello and then close the connection.
  console.log("Connection from " + socket.remoteAddress);
  socket.end("Hello World\n");
});

// Fire up the server bound to port 7000 on localhost
server.listen(7000, "localhost");
