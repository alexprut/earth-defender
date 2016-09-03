// Global variables, just for debuging use
// @FIXME bad security
var game;

// Initialise the game when the page is loaded completely
document.body.onload = function () {
    console.log("Game Page loaded");
    game = new Game({
        'maxLife': 1000,
        'maxMeteorietes': 200,
        'isMultiplayer': true,
        'maxPlayers': 10,
        'debug': false,
        'servers': ['158.110.224.100:8888/websocket']
    });
    game.init();
    game.stop();
};
