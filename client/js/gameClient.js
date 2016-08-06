var GameClient = function (config) {
    this.servers = config.servers;
    this.game = config.game;
    this.websocket = null;
};
GameClient.prototype.constructor = GameClient;
GameClient.prototype.connect = function () {
    this.websocket = new WebSocket('ws://' + this.servers);

    this.websocket.onopen = this.onOpen;
    this.websocket.onclose = this.onClose;
    this.websocket.onerror = this.onError;
    this.websocket.onmessage = this.onMessage;
};
GameClient.prototype.disconnect = function () {
    this.send("rem_player");
    this.send("ret_player");

    websocket.close();
};
GameClient.prototype.send = function (data) {
    if (websocket.readyState === websocket.OPEN) {
        websocket.send(data);
        console.log('sending: ' + data);
    } else {
        console.log('WebSocket is not connected');
    }
};
GameClient.prototype.onOpen = function (event) {
    console.log('onOpen');

    this.send("add_player");
    this.send("ret_player");
};
GameClient.prototype.onClose = function (event) {
    console.log('onClose');

    this.send("rem_player");
    this.send("ret_player");
};
GameClient.prototype.onMessage = function (event) {
    console.log('onMessage' + event);

    game.setPlayers(event.data);
};
GameClient.prototype.onError = function (event) {
    console.log('onError ' + event.data);

    this.send("rem_player");
    this.send("ret_player");
};
