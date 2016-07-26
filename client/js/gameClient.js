var connection = new WebSocket('ws://127.0.0.1:8888');

connection.onopen = function () {
    connection.send("Ping");
    console.log("asdasd");
};

connection.onerror = function (error) {
    console.log(error);
};

connection.onmessage = function (message) {
    console.log(message);
};
