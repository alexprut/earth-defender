var GameClient = function (config) {
    this.servers = config.servers;
    this.game = config.game;
    this.websocket = null;
    this.queue = [];
    this.heartBeatInterval = 60000; // 1 minute
    this.heartBeatId = null;
};
GameClient.prototype.constructor = GameClient;
GameClient.prototype.connect = function () {
    this.websocket = new WebSocket('ws://' + this.servers);

    this.websocket.onopen = this.onOpen.bind(this);
    this.websocket.onclose = this.onClose.bind(this);
    this.websocket.onerror = this.onError.bind(this);
    this.websocket.onmessage = this.onMessage.bind(this);
};
GameClient.prototype.disconnect = function () {
    this.send("player_remove");
    this.send("players_get");

    this.websocket.close();
};
GameClient.prototype.stopHeartBeat = function () {
    clearTimeout(this.heartBeatId);
};
GameClient.prototype.resetHeartBeat = function () {
    this.stopHeartBeat();

    this.heartBeatId = setTimeout((function () {
        if (this.isConnected()) {
            this.send("ping", null);
            this.resetHeartBeat();
        }
    }).bind(this), this.heartBeatInterval);
};
GameClient.prototype.isConnected = function () {
    return (this.websocket && (this.websocket.readyState === this.websocket.OPEN)) ? true : false;
};
GameClient.prototype.send = function (event, data) {
    var sendData = JSON.stringify([event, data]);
    if (this.isConnected()) {
        this.websocket.send(sendData);
        console.log('sending:');
        console.log(sendData);
    } else {
        console.log('WebSocket is not connected: message queued');
        console.log('Event: ' + event + " Data:" + data);
        this.queue.push({event: event, data: data});
    }
};
GameClient.prototype.onOpen = function (event) {
    console.log('onOpen');
    var sendData;
    while (sendData = this.queue.shift()) {
        this.send(sendData.event, sendData.data);
    }

    this.resetHeartBeat();
};
GameClient.prototype.onClose = function (event) {
    console.log('onClose');
};
GameClient.prototype.onMessage = function (event) {
    var receivedData = JSON.parse(event.data);
    var action = receivedData[0];
    var data = (receivedData.length > 1) ? receivedData[1] : null;

    console.log('onMessage (Receiving)');
    console.log(event);

    switch (action) {
        case "room_players_number":
            this.game.setPlayers(data);
        case "player_id":
            break;
        case "room_id":
            this.game.roomId = data;
            break;
        case "rooms_list":
            this.game.setRoomsList(data);
            break;
        case "game_life":
            this.game.setLife(data);
            break;
        case "asteroid_position":
            var tmpData = [];
            for (var i = 0, length = this.game.meteorites.children.length; i < length; i++) {
                var position = this.game.getMeteoritePosition(i);
                tmpData.push([position.x, position.y, position.z]);
            }
            this.send("game_master_asteroids_position", tmpData);
            break;
        case "asteroid_position_set":
            this.game.setAsteroidPosition(data);
            break;
        case "ship_position_set":
            if (data.length > 1)
                this.game.addNewSpaceShip(data);
            this.game.setPositionSpaceShip(data);
            break;
        case "ship_shoot":
            this.game.shoot_online(data);
            break;
        case "pong":
            break;
        default:
            console.log("Warning: onMessage can not handle action " + "\"" + action + "\"");
    }
};
GameClient.prototype.onError = function (event) {
    console.log('onError');
    console.log(event.data);
};
