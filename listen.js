// this file is used to keep a image alive so that it can be pre-warmed and communicated with.
var net = require('net');

// Creates a new TCP server. The handler argument is automatically set as a listener for the 'connection' event
var server = net.createServer(function(socket) {
  // Every time someone connects, tell them hello and then close the connection.
  console.log("Connection from " + socket.remoteAddress);
  socket.end("Hello World\n");
});

// Fire up the server bound to port 7000 on localhost
server.listen(7000, "localhost");
