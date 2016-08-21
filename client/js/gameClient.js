var GameClient = function (config) {
    this.servers = config.servers;
    this.game = config.game;
    this.websocket = null;
    this.queue = [];
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
GameClient.prototype.send = function (event, data) {
    var sendData = JSON.stringify([event, data]);
    if (this.websocket && (this.websocket.readyState === this.websocket.OPEN)) {
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
};
GameClient.prototype.onClose = function (event) {
    console.log('onClose');
    this.send("player_remove");
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
            var data = [];
            var tmp = this.game.meteorites.children;
            for(var i = 0; i < this.game.meteorites.children.length; i++){
                data.push([tmp[i].position.x,tmp[i].position.y,tmp[i].position.z]);
               }
            this.send("master_asteroid_position",data);
            break;
        case "asteroid_position_set":
            this.game.setAsteroidPosition(data);
            break;
        case "ship_position_set":
            if(data.length>1)
                this.game.addNewSpaceShip(data);
            this.game.setPositionSpaceShip(data);
            break;
        case "ship_shoot":
            this.game.shoot_online(data);
        default:
            console.log("Warning: onMessage can not handle action " + "\"" + action + "\"");
    }
};
GameClient.prototype.onError = function (event) {
    console.log('onError');
    console.log(event.data);
};
