// Library for generating random strings
var randomString = require("randomstring");

// Pubblish / Subscribe design pattern
function PublishSubscribe() {
    var subscribers = {
        topics: [],
        token: -1,
        add: function (event, func) {
            if (!this.topics[event]) {
                this.topics[event] = [];
            }

            this.topics[event][++this.token] = func;

            return (this.token).toString();
        },
        remove: function (token) {
            for (var event in this.topics) {
                if (this.topics[event][token]) {
                    this.topics[event].splice(token, 1);

                    return true;
                }
            }

            return false;
        },
        broadcast: function (event, args) {
            for (var subscriber in this.topics[event]) {
                this.topics[event][subscriber](args);
            }
        }
    };

    return {
        subscribe: function (event, func) {
            return subscribers.add(event, func);
        },
        unsubscribe: function (token) {
            return subscribers.remove(token);
        },
        publish: function (event, args) {
            subscribers.broadcast(event, args);
        }
    }
};

function Clients() {
    var _clients = [];

    function remove(clientId) {
        var index = _clients.indexOf(clientId);
        if (index != -1) {
            _clients.splice(index, 1)
        }
    }

    function add(clientId) {
        _clients.push(clientId);
    }

    function getAll () {
        return _clients;
    }

    return {
        add: add,
        remove: remove,
        getAll: getAll
    }
}


var clients = new Clients();
var gameRoom = new PublishSubscribe();


/////////////////////////////////////////////////////
// WEBSOCKET SERVER & HTTP SERVER
/////////////////////////////////////////////////////
var server = require('http').createServer()
    , url = require('url')
    , WebSocketServer = require('ws').Server
    , wss = new WebSocketServer({server: server})
    , express = require('express')
    , app = express()
    , port = 8888;

app.use(function (req, res) {
    res.send({msg: "Earth Defender: Game Server"});
});

wss.on('connection', function connection(ws) {
    const clientId = randomString.generate(10);

    clients.add(clientId);
    ws.send(JSON.stringify({'clientId': clientId}));

    ws.on('message', function incoming(message) {
        console.log('received: %s', message);
        console.log(message);
        console.log(clients.getAll());
    });

    ws.on('close', function (error) {
        clients.remove(clientId);
        console.log("closed connection client: " + clientId);
    });
});

server.on('request', app);
server.listen(port, function () {
    console.log('Listening on ' + server.address().port)
});
