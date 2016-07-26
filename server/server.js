var randomString = require("randomstring");
var clients = [];
var gameRoom = [];


/////////////////////////////////////////////////////
// WEBSOCKET SERVER & HTTP SERVER
/////////////////////////////////////////////////////
var server = require('http').createServer()
    , url = require('url')
    , WebSocketServer = require('ws').Server
    , wss = new WebSocketServer({ server: server })
    , express = require('express')
    , app = express()
    , port = 8888;

app.use(function (req, res) {
    res.send({ msg: "Earth Defender: Game Server" });
});

wss.on('connection', function connection(ws) {
    var clientId = randomString.generate(10);
    clients.push(clientId);
    ws.send(JSON.stringify({'clientId': clientId}));

    ws.on('message', function incoming(message) {
        console.log('received: %s', message);
        console.log(message);
        console.log(clients);
    });

    ws.on('close', function (error) {
       console.log("closed connection client: " + clientId);
    });
});

server.on('request', app);
server.listen(port, function () { console.log('Listening on ' + server.address().port) });
