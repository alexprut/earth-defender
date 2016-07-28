var game;
// Init the game when the page is loaded completely
document.body.onload = function () {
    console.log("Game Page loaded");
    game = new Game();
    game.init();
};


