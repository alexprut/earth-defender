var websocket;
var player_socket;
init();

function init() {
    connect();
};

function connect() {
    websocket = new WebSocket('ws://127.0.0.1:8888/websocket');
    websocket.onopen = function (evt) {
        onOpen(evt)
    };
    websocket.onclose = function (evt) {
        onClose(evt)
    };
    websocket.onmessage = function (evt) {
        onMessage(evt)
    };
    websocket.onerror = function (evt) {
        onError(evt)
    };
};

function disconnect() {
    websocket.close();
};

function sendTxt(txt) {
    if (websocket.readyState == websocket.OPEN) {
        websocket.send(txt);
        console.log('sending: ' + txt);
    } else {
        console.log('websocket is not connected');
    }
};

function onOpen(evt) {
    console.log('onOpen');
    game.updateNofPlayers();
};

function onClose(evt) {
    console.log('onClose');
};

function onMessage(evt) {
    player_socket = evt.data;
    game.updateNofPlayers();
};

function onError(evt) {
    console.log('onError' + evt.data);
};
