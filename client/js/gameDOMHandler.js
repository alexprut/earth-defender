/////////////////////////////////////////////////////
// Game DOM Handler
/////////////////////////////////////////////////////
Element.prototype.remove = function () {
    this.parentElement.removeChild(this);
};

var GameDOMHandler = function (gameHandler) {
    (function updateRendererSize() {
        document.body.addEventListener("resize", function () {
            gameHandler.setRendererSize(window.innerWidth, window.innerHeight);
        })
    })();

    function setLife(life) {
        document.getElementById('life').innerHTML = life;
    }

    function setPlayers(players) {
        document.getElementById('players').innerHTML = players;
    }

    function setScore(score) {
        document.getElementById('score').innerHTML = score;
    }

    function updateRoomsList(roomsList) {
        var handler = document.getElementById('gameRoom-list');
        handler.innerHTML = "";

        if (roomsList.length === 0) {
            var li = document.createElement("li");
            li.innerHTML = "No results";
            handler.appendChild(li);
        } else {
            var roomId;
            while (roomId = roomsList.shift()) {
                var li = document.createElement("li");
                li.setAttribute("data-roomid", roomId);
                li.innerHTML = roomId;
                handler.appendChild(li);
            }
        }
    }

    function showComponentGameType() {
        hideComponentGameRoom();
        document.getElementById('gameType').className = '';
    }

    function hideComponentGameType() {
        document.getElementById('gameType').className = 'hidden';
    }

    function showComponentGameRoom() {
        hideComponentGameType();
        document.getElementById('gameRoom').className = '';
    }

    function hideComponentGameRoom() {
        document.getElementById('gameRoom').className = 'hidden';
    }

    // Return the id of the selected room to join in multiplayer mode, otherwise return null
    function getSelectedRoom() {
        var tmp = document.getElementById('gameRoom-list').getElementsByTagName('li');
        for (var i = 0; i < tmp.length; i++) {
            if (tmp[i].className = "selected" && tmp[i].getAttribute('data-roomid')) {
                return tmp[i].getAttribute('data-roomid');
            }
        }

        return null;
    }

    function init() {
        if (!gameHandler.isMultiplayer) {
            hideComponentGameType();
        } else {
            document.getElementById('gameType-singlePlayer').addEventListener('click', function () {
                console.log("Single Player game type selected");
                hideComponentGameType();
                gameHandler.stop("Loading ...");
                gameHandler.start();
            });

            document.getElementById('gameType-multiPlayer').addEventListener('click', function () {
                console.log("Multiplayer game type selected");
                gameHandler.server.connect();
                gameHandler.server.send('rooms_list');
                showComponentGameRoom();

                document.getElementById('gameRoom-join').addEventListener('click', function () {
                    // Control if a room was selected, otherwise just do nothing when the Join button is pressed
                    if (getSelectedRoom()) {
                        hideComponentGameRoom();
                        gameHandler.server.send('room_join', getSelectedRoom());
                        gameHandler.stop("Loading ...");
                        gameHandler.start();

                        var position = gameHandler.getSpaceShipPosition(index);
                        gameHandler.server.send("game_ship_position", [
                            index,
                            position.x,
                            position.y,
                            position.z
                        ]);
                    }
                });

                document.getElementById('gameRoom-create').addEventListener('click', function () {
                    hideComponentGameRoom();
                    gameHandler.server.send('room_add');
                    gameHandler.stop("Loading ...");
                    gameHandler.start();

                    var position = gameHandler.getSpaceShipPosition(index);
                    gameHandler.server.send("game_ship_position", [
                        index,
                        position.x,
                        position.y,
                        position.z
                    ]);
                });

                document.getElementById('gameRoom-list').addEventListener('click', function (e) {
                    if (e.target && e.target.nodeName == "LI") {
                        // Deselect all other nodes
                        var tmp = document.getElementById('gameRoom-list').getElementsByTagName('li');
                        for (var i = 0; i < tmp.length; i++) {
                            tmp[i].className = "";
                        }

                        // Select current node
                        e.target.className = "selected";
                    }
                });
            });
        }

        while (document.getElementsByTagName('canvas').length) {
            document.getElementsByTagName('canvas')[0].remove();
        }

        document.body.appendChild(gameHandler.renderer.domElement);

        clearMessage();
        resetLife();
        resetScore();
    }

    function resetLife() {
        setLife(gameHandler.maxLife);
    }

    function resetScore() {
        setScore(0);
    }

    function setMessage(msg) {
        document.getElementById('message').className = '';
        document.getElementById('message').innerHTML = msg;
    }

    function clearMessage() {
        document.getElementById('message').className = 'hidden';
        document.getElementById('message').innerHTML = '';
    }

    // Module Pattern
    return {
        init: init,
        setLife: setLife,
        setPlayers: setPlayers,
        setScore: setScore,
        resetLife: resetLife,
        resetScore: resetScore,
        setMessage: setMessage,
        clearMessage: clearMessage,
        updateRoomsList: updateRoomsList,
        getSelectedRoom: getSelectedRoom
    }
};
